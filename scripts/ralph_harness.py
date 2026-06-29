#!/usr/bin/env python3
import os
import sys
import json
import re
import subprocess

STATE_FILE = "scratch/ralph_state.json"
MARKERS = ["placeholder", "placeholders", "stub", "stubs", "stubbed", "todo", "todos", "no-op", "simplified", "not yet"]

def find_port_debt():
    debt_by_file = {}
    
    # Traverse src and app directories
    for root_dir in ["src", "app"]:
        if not os.path.exists(root_dir):
            continue
        for root, _, files in os.walk(root_dir):
            for file in files:
                if not file.endswith(".hs"):
                    continue
                path = os.path.join(root, file)
                try:
                    with open(path, "r", encoding="utf-8", errors="ignore") as f:
                        lines = f.readlines()
                except Exception as e:
                    print(f"Warning: Could not read {path}: {e}")
                    continue
                
                file_markers = []
                for idx, line in enumerate(lines):
                    line_no = idx + 1
                    lower_line = line.lower()
                    
                    # Tokenize line to check for exact word match of markers (except 'not yet' which is substring)
                    words = re.findall(r'[a-zA-Z0-9\-]+', lower_line)
                    has_marker = "not yet" in lower_line or any(m in words for m in MARKERS)
                    
                    if has_marker:
                        file_markers.append({
                            "line": line_no,
                            "content": line.strip()
                        })
                
                if file_markers:
                    debt_by_file[path] = file_markers
                    
    return debt_by_file

def load_state():
    if os.path.exists(STATE_FILE):
        try:
            with open(STATE_FILE, "r", encoding="utf-8") as f:
                return json.load(f)
        except Exception as e:
            print(f"Error loading state: {e}. Reinitializing...")
    return {"tasks": [], "current_task_id": None}

def save_state(state):
    os.makedirs(os.path.dirname(STATE_FILE), exist_ok=True)
    with open(STATE_FILE, "w", encoding="utf-8") as f:
        json.dump(state, f, indent=2)

def cmd_init():
    debt = find_port_debt()
    state = load_state()
    
    existing_tasks = {t["file"]: t for t in state.get("tasks", [])}
    new_tasks = []
    task_id = 1
    
    # Sort files to be deterministic
    for file_path in sorted(debt.keys()):
        markers = debt[file_path]
        if file_path in existing_tasks:
            # Update existing task markers but preserve status
            task = existing_tasks[file_path]
            task["markers"] = markers
            new_tasks.append(task)
        else:
            new_tasks.append({
                "id": task_id,
                "file": file_path,
                "markers": markers,
                "status": "pending",
                "attempts": 0,
                "notes": ""
            })
        task_id += 1
        
    # Re-index task IDs to be contiguous
    for idx, task in enumerate(new_tasks):
        task["id"] = idx + 1
        
    state["tasks"] = new_tasks
    save_state(state)
    print(f"Initialized {len(new_tasks)} tasks from port-debt markers.")

def cmd_status():
    state = load_state()
    tasks = state.get("tasks", [])
    if not tasks:
        print("No tasks initialized. Run 'init' first.")
        return
        
    completed = [t for t in tasks if t["status"] == "completed"]
    failed = [t for t in tasks if t["status"] == "failed"]
    skipped = [t for t in tasks if t["status"] == "skipped"]
    pending = [t for t in tasks if t["status"] == "pending"]
    
    print("\n--- Ralph Oracle Harness Status ---")
    print(f"Total Tasks:     {len(tasks)}")
    print(f"Completed:       {len(completed)}")
    print(f"Failed:          {len(failed)}")
    print(f"Skipped:         {len(skipped)}")
    print(f"Pending:         {len(pending)}")
    
    current_id = state.get("current_task_id")
    if current_id:
        current = next((t for t in tasks if t["id"] == current_id), None)
        if current:
            print(f"Current Task:    #{current['id']} - {current['file']} ({current['status']})")
            
    print("\nPending Tasks:")
    for t in pending[:10]:
        print(f"  #{t['id']}: {t['file']} ({len(t['markers'])} markers)")
    if len(pending) > 10:
        print(f"  ... and {len(pending) - 10} more.")

def cmd_next():
    state = load_state()
    tasks = state.get("tasks", [])
    
    # If there is a current task that is still pending or failed, keep it active
    current_id = state.get("current_task_id")
    current = None
    if current_id:
        current = next((t for t in tasks if t["id"] == current_id), None)
        if current and current["status"] not in ["completed", "skipped"]:
            pass
        else:
            current = None
            
    if not current:
        # Find the first pending or failed task
        current = next((t for t in tasks if t["status"] == "pending"), None)
        if not current:
            current = next((t for t in tasks if t["status"] == "failed"), None)
            
    if not current:
        print("No more pending tasks! All completed or skipped.")
        state["current_task_id"] = None
        save_state(state)
        return
        
    state["current_task_id"] = current["id"]
    save_state(state)
    
    # Print task description in a way that is easily readable by the worker agent
    print(f"\n=========================================")
    print(f"ACTIVE TASK: #{current['id']}")
    print(f"FILE: {current['file']}")
    print(f"STATUS: {current['status']}")
    print(f"ATTEMPTS: {current['attempts']}")
    print(f"MARKERS:")
    for m in current["markers"]:
        print(f"  Line {m['line']}: {m['content']}")
    print(f"=========================================\n")
    print("INSTRUCTION FOR AGENT:")
    print(f"Please inspect [file://{os.path.abspath(current['file'])}] and resolve the stubs/placeholders/todos/simplifications listed above.")
    print("Verify your edits by running the oracle: './scripts/ralph_harness.py oracle'.")

def cmd_oracle():
    state = load_state()
    current_id = state.get("current_task_id")
    if not current_id:
        print("No active task set. Run './scripts/ralph_harness.py next' first.")
        sys.exit(1)
        
    tasks = state.get("tasks", [])
    current = next((t for t in tasks if t["id"] == current_id), None)
    if not current:
        print(f"Task #{current_id} not found.")
        sys.exit(1)
        
    current["attempts"] += 1
    save_state(state)
    
    print("Running oracle (stack test)...")
    res = subprocess.run(["stack", "test"], capture_output=True, text=True)
    
    # Output stderr and stdout
    print(res.stdout)
    if res.returncode != 0:
        print(res.stderr, file=sys.stderr)
        print("\n❌ Oracle FAILED.")
        current["status"] = "failed"
        save_state(state)
        sys.exit(1)
    else:
        print("\n✅ Oracle PASSED (all tests succeeded).")
        current["status"] = "completed"
        save_state(state)
        
        # Git diff check
        diff_res = subprocess.run(["git", "diff", "--quiet", current["file"]])
        if diff_res.returncode == 0:
            print("No changes detected in task file. Skipping git commit.")
        else:
            print("Changes detected. Committing changes...")
            subprocess.run(["git", "add", current["file"]])
            subprocess.run(["git", "commit", "-m", f"Resolve port debt in {current['file']} via Ralph Loop"])
            print("Committed successfully.")

def cmd_skip():
    state = load_state()
    current_id = state.get("current_task_id")
    if not current_id:
        print("No active task to skip.")
        return
    tasks = state.get("tasks", [])
    current = next((t for t in tasks if t["id"] == current_id), None)
    if current:
        current["status"] = "skipped"
        print(f"Task #{current['id']} skipped.")
        state["current_task_id"] = None
        save_state(state)

def main():
    if len(sys.argv) < 2:
        print("Usage: ./scripts/ralph_harness.py [init | status | next | oracle | skip]")
        sys.exit(1)
        
    cmd = sys.argv[1]
    if cmd == "init":
        cmd_init()
    elif cmd == "status":
        cmd_status()
    elif cmd == "next":
        cmd_next()
    elif cmd == "oracle":
        cmd_oracle()
    elif cmd == "skip":
        cmd_skip()
    else:
        print(f"Unknown command: {cmd}")
        sys.exit(1)

if __name__ == "__main__":
    main()
