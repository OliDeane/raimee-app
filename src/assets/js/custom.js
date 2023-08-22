document.addEventListener('mouseup', () => {
    var sel = window.getSelection();
    var sel_str = sel.toString();
    var target_value = '';
    if (sel_str.length > 0) {
      var pid = sel.focusNode.parentNode.id;
      if (pid == 'selection-container') {
        target_value = sel_str;
      }
    }
    // Way to set value of React input
    var input = document.getElementById('selection-target');
    var nativeInputValueSetter = Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, "value").set;
    nativeInputValueSetter.call(input, target_value);
    var ev2 = new Event('input', {bubbles: true});
    input.dispatchEvent(ev2);
  
  });