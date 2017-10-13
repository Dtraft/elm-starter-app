import './main.css';
import { Main } from './Main.elm';

var token = localStorage.getItem("token");
var app = Main.embed(document.getElementById('root'), {
    token: token ? token : "test"
});

app.ports.setToken.subscribe(function(token) {
    try{
        localStorage.setItem("token", token);
    }catch (e){
        console.log(e);
    }
});
