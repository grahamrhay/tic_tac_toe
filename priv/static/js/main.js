var sessionId, gameId;

var newGameBtn = document.getElementById('new-game');

var socket = new WebSocket('ws://localhost:8080/connect');

socket.onopen = function() {
    send('new_session');
};

socket.onmessage = function(ev) {
    console.log('Received data: ' + ev.data);
    var msg = JSON.parse(ev.data);
    if (msg.type === 'new_session') {
        sessionId = msg.id;
        newGameBtn.disabled = false;
    } else if (msg.type === 'new_game') {
        gameId = msg.id;
        updateStatus('New game!');
        clearBoard();
    } else if (msg.type === 'your_turn') {
        updateStatus('Your turn!');
        updateBoard(msg.data);
        enableBoard();
    } else if (msg.type === 'wait') {
        updateBoard(msg.data);
        updateStatus('Waiting for other player...');
    }
};

newGameBtn.onclick = function() {
    var msg = JSON.stringify({type: 'new_game', sessionId: sessionId});
    send(msg);
    newGameBtn.disabled = true;
    updateStatus('Waiting to join game...');
};

function clearBoard() {
    for (var i = 1; i < 10; i++) {
        document.getElementById('cell' + i).innerHTML = '';
    }
}

function updateStatus(status) {
    document.getElementById('status').innerHTML = status;
}

function send(msg) {
    console.log('Sent msg: ' + msg);
    socket.send(msg);
}

function enableBoard() {
    for (var i = 1; i < 10; i++) {
        var cell = document.getElementById('cell' + i);
        cell.onclick = (function(c) {
            return function() {
                var row = c.dataset.row;
                var cell = c.dataset.cell;
                var msg = JSON.stringify({type: 'play', gameId: gameId, cell: row + ',' + cell});
                send(msg);
                disableBoard();
            };
        })(cell);
        cell.classList.add('active');
    }
}

function disableBoard() {
    for (var i = 1; i < 10; i++) {
        var cell = document.getElementById('cell' + i);
        cell.onclick = null;
        cell.classList.remove('active');
    }
}

function updateBoard(data) {
    for (var i = 0; i < 9; i++) {
        var row = Math.floor(i / 3) + 1;
        var cell = (i % 3) + 1;
        var symbol = data[row + ',' + cell];
        if (symbol === '_') {
            symbol = '';
        }
        document.getElementById('cell' + (i + 1)).innerHTML = symbol;
    }
}
