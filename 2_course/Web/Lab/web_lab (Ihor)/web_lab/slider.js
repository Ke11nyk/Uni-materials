

function getCurrentBtnID(){
    var cur_btn;
    for(let i = 1; i <= 5; i++){
        cur_btn = document.getElementById(`sb${i}`);

        if(cur_btn.checked){
            return i;
        }
    }
}

setInterval(() => {
    var cur_btn_id = getCurrentBtnID();
    if(cur_btn_id === 5){
        cur_btn_id = 1;
    }
    else{
        cur_btn_id += 1;
    }
    var cur_btn = document.getElementById(`sb${cur_btn_id}`);
    cur_btn.checked = true;
}, 4000);
