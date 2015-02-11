

var output = "<ul>";

for( var i in data.users){
  output += "<li>" +
    data.users[i].firstName + " " 
  + data.users[i].lastName 
  + ", since (" 
  + data.users[i].joined
  + ")"
  + "</li>";
  
}

output += "</ul>";
document.getElementById("x").innerHTML = output;
