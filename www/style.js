const clipButton = document.getElementById('clipbtn');

clipButton.addEventListener('click', function onClick(event) {
  event.target.style.backgroundColor = 'green';
  event.target.style.color = 'white';
});
