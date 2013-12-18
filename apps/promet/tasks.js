// In memory tasks store
tasks = [];
// This will change the view if localStorage isn't available. It
// returns true or false
function verifyLocalStorage() {
  if (!window.localStorage) {
    document.querySelector('.section').style.display="none";
    document.querySelector('#not-supported').style.display="block";
    return false;
  }
  return true;
}
function DeleteDoneTasks(){
  var i = 0;
  var list = document.querySelector('#task-list');
  list.innerHTML = "";
  var aTask = tasks[i];
  while (aTask) {
    var bTask = tasks[i+1];
    if (aTask.COMPLETED=='Y'){
      tasks.splice(i,1);
    } else {
      i++;
    }
    aTask = bTask;
  }
  localStorage['tasks'] = JSON.stringify(tasks);
  delete tasks;
  tasks = new Array();
  LoadTasks();
}
function TaskDone(event){
  e = event.target;
  aTask = tasks[e.parentElement.id];
  if (e.checked)
    aTask.COMPLETED='Y';
  else
    aTask.COMPLETED='N';
  localStorage['tasks'] = JSON.stringify(tasks);
}
// This adds a task
function addTask(task){
  // To the view
  var nli=document.createElement("li");
  var list = document.querySelector('#task-list');
  list.appendChild(nli);
  var ndiv = document.createElement("div");
  nli.appendChild(ndiv);
  ndiv.setAttribute('id',task.LPRIORITY);
  ndiv.innerHTML = '<input type="checkbox" style="width:auto;display:inline;"><label style="margin-left:5px;">'+task.SUMMARY+'</label>';
  ndiv.firstElementChild.checked = (task.COMPLETED=='Y');
  // To memory
  tasks[tasks.length] = task;
  ndiv.firstElementChild.addEventListener('CheckboxStateChange',TaskDone);
}
function LoadTasks() {
// Set it all up
// Notify the user if local storage isn't supported
if (verifyLocalStorage() == true) {
    // Get the last stored tasks and restore them to the UI.
    // Default to an empty array
    var jsoncode = localStorage.getItem('tasks');
    var oldTasks = JSON.parse(jsoncode || '[]');
    for (var i=0;i<=oldTasks.length-1;i++){
      oldTasks[i].lpriority = i;
      addTask(oldTasks[i]);
    }
}
}
LoadTasks();
// Set up a handler for submission
document.querySelector('#submitForm').onsubmit=function(){
  // Add the new task
  var aTaskName = document.querySelector('#task-name');
  var newTask = { 'SUMMARY' : aTaskName.value,
                  'SQL_ID' : undefined,
                  'COMPLETED' : 'N',
                  'LPRIORITY' : 0
                };
  newTask.lpriority = tasks.length;
  addTask(newTask);
  // In storage
  localStorage['tasks'] = JSON.stringify(tasks);
  // Clear the input
  document.querySelector('#task-name').value = '';
  // Don't post
  return false;
}
if (IsConnectionOK){
  GetList("tasks","\"HASCHILDS\"<>\'Y\'",function (aData)
    {
    var RemoteTasks = aData;
    for (var r=0;r<=RemoteTasks.length-1;r++){
      var found = false;
      for (var i=0;i<=tasks.length-1;i++) {
        if (tasks[i].SQL_ID==RemoteTasks[r].SQL_ID)
          {
            if (tasks[i].TIMESTAMPD>RemoteTasks[r].TIMESTAMPD) {
              //sync out
            } else {
              //sync in
              tasks[i].SUMMARY = RemoteTasks[r].SUMMARY;
            }

            found = true;
          }
      }
      if (!found)
        addTask(RemoteTasks[r]);
    }
  }
  );
}

