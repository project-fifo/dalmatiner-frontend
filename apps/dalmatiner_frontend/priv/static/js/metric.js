$(function() {
    msgpack.download("", {header: {accept:"application/x-msgpack"}}, function(d) {
        console.log(d);
        d.forEach(function(e) {
            $("#metrics").append("<tr><td>" + e +"</td></tr>")
        })
    })
})