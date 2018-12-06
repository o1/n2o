var protocol = location.protocol == 'https:' ? 'wss://' : 'ws://';
var port = location.port == '' ? '' : (':' + location.port);
var ws = new WebSocket(protocol + location.hostname + port + '/ws');
ws.onopen = () => {
    ws.onmessage = (e) => {
        document.getElementById('echo').innerText=e.data;
    };
};
document.getElementById('submit').addEventListener('submit', (e) => {
    e.stopPropagation();
    e.preventDefault();
    ws.send(document.getElementById('message').value);
});
