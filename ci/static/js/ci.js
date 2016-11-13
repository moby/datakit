function highlight_log() {
	var current = document.getElementById('iframe_log').contentWindow.location.href;
	console.debug("current:" + current);
	var elements = document.getElementsByClassName('log-link');
	for (x = 0; x < elements.length; x++) {
		if (elements[x].href == current) {
			console.debug("set:" + elements[x]);
			elements[x].className = "selected-log log-link"
		} else {
			console.debug("unset:" + elements[x]);
			elements[x].className = "log-link"
		}
	}
}
