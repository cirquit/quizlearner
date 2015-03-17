window.onkeypress = function(e) {
      var key = e.keyCode ? e.keyCode : e.which;
      if (key == 37) {
          back();
      }else if (key == 39) {
          next();
      }else if (key == 13) {
         $('#submitId').parent("form").submit();
      }
   }
   function next() {
       var radios = document.getElementsByName('tabs');
       for(i = 0; i < radios.length; i++){
           if(radios[i].checked){
               radios[i+1].checked = true;
               return;
           }
       }
       radios[0].checked = true;
       return;
   }
   function back() {
       var radios = document.getElementsByName('tabs');
       for(i = 0; i < radios.length; i++){
           if(radios[i].checked){
               radios[i-1].checked = true;
               return;
           }
       }
       radios[0].checked = true;
       return;
   }