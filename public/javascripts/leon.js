var editor = null;

$(document).ready(function() {
    editor = ace.edit("codebox");
    editor.getSession().setMode("ace/mode/scala");
    editor.getSession().setUseWrapMode(true);

    var hash = window.location.hash

    var WS = window['MozWebSocket'] ? MozWebSocket : WebSocket
    var leonSocket = new WS(_leon_websocket_url)

    var handlers = [];
    var compilationStatus = 0
    var searchFinished = false
    var verificationFinished = false
    var context = "unknown";

    handlers["compilation"] = function (data) {
        var e = $("#compilation-status")
        e.hide();
        if(data.status == "success") {
            e.attr("class", "success")
            compilationStatus = 1
            e.html("Success")
        } else {
            e.attr("class", "failure")
            compilationStatus = -1
            e.html("Error")
        }
        e.show();
    }

    handlers["editor"] = function (data) {
        if ("annotations" in data) {
            var session = editor.getSession();

            context = "unknown";

            for (var i = 0; i < data.annotations.length; i++) {
                var a = data.annotations[i];
                if (a.type == "verification") {
                    context = "verification";
                } else if (a.type == "synthesis") {
                    context = "synthesis";
                }
                console.log(a.type);

                if (a.type != "info" && a.type != "error") {
                    session.addGutterDecoration(a.row, "leon_gutter_"+a.type)
                    a.type = "info";
                }
            }

            session.setAnnotations(data.annotations);
        }
    }

    handlers["notification"] = function (data) {
        notify(data.content, data.type);
    }

    handlers["log"] = function (data) {
        var txt = $("#console textarea")
        txt.val(txt.val()+"\n"+data.message);
        txt.scrollTop(txt[0].scrollHeight - txt.height())
    }

    handlers["synthesis_search"] = function (data) {
        if (data.action == "progress") {
            var pc = (data.closed*100)/data.total;
            $("#searchProgress").progressbar("value", pc)
            $("#searchProgress .progress-label").text(data.closed + " / "+data.total)
        } else if (data.action == "result") {
            searchFinished = true
            $("#searchDialog").dialog("close");
        }
    }

    handlers["verification_result"] = function (data) {
        $("#verifyProgress").progressbar("value", 100)
        var pbValue = $("#verifyProgress").find(".ui-progressbar-value");

        if (data.status == "success") {
            $("#verifyProgress .progress-label").text("Success!")
            pbValue.addClass("success")
        } else {
            $("#verifyProgress .progress-label").text("Failed!")
            pbValue.addClass("failure")
        }
        verificationFinished = true

        var tbl = $("#verifyResults tbody")
        tbl.html("");

        for (var i = 0; i < data.vcs.length; i++) {
            var vc = data.vcs[i];
            var icon = "check"
            if (vc.status == "invalid") {
                icon = "alert"    
            } else if (vc.status == "unknown") {
                icon = "help"    
            }


            tbl.append("<tr class=\""+((i%2 == 0) ? "odd " : "")+vc.status+"\"> <td>"+vc.fun+"</td> <td>"+vc.kind+"</td> <td><span class=\"ui-icon ui-icon-"+icon+"\"></span>"+vc.status+"</td> <td>"+vc.time+"</td> </tr>")

            if ("counterExample" in vc) {
                var html = "<tr class=\""+((i%2 == 0) ? "odd " : "")+"counter-example\"><td colspan=\"4\"><div><div>The following example violates the VC:</div><table>";

                for (var v in vc.counterExample) {
                    html += "<tr><td>"+v+"</td><td><span class=\"ui-icon ui-icon-arrowthick-1-e\"></span></td><td>"+vc.counterExample[v]+"</td></tr>";
                }
                html += "</div></td></tr></table>"

                tbl.append(html)
            }
        }


        $("div[aria-describedby='verifyDialog'] span.ui-button-text").html("Close")
        $("#verifyResults").show("fade");
    }

    function error(msg) {
        alert(msg);
    }

    var receiveEvent = function(event) {
        var data = JSON.parse(event.data)
        if (data.kind in handlers) {
            handlers[data.kind](data);
        } else {
            error("Unknown event type: "+data.kind)
        }
    }

    leonSocket.onmessage = receiveEvent

    var lastChange = new Date().getTime();
    var timeWindow = 1000;

    function notify(content, type) {

        var note = $("<div>", {
            "class": type
        }).html(content)

        note.hide();

        $("#notifications").append(note);
        note.show("fade");
        setTimeout(function() {
            note.hide("fade");
        }, 2000)
    }

    function recompile() {
        var e = $("#compilation-status")
        e.attr("class", "")
        compilationStatus = 0
        e.html("<img src=\""+_leon_loader_url+"\" />")

        var msg = JSON.stringify(
          {action: "doUpdateCode", code: editor.getValue()}
        )

        leonSocket.send(msg)
    }

    function onCodeUpdate() {
        var now = new Date().getTime()

        if (lastChange < (now - timeWindow)) {
            recompile()
        }

        localStorage.setItem("leonEditorCode", editor.getValue());
    }

    function loadExample() {
        var selected = $('#example-loader').find(":selected")

        var group = selected.attr("group")
        var value = selected.attr("id")

        if (value) {
            $.ajax({
              url: _leon_prefix+'/ajax/getExample/'+group+'/'+value,
              dataType: "json",
              success: function(data, textStatus, jqXHR) {
                if (data.status == "success") {
                    editor.setValue(data.code);
                    editor.selection.clearSelection();
                    editor.gotoLine(0);
                    recompile();
                    $("#example-loader").get(0).selectedIndex = 0;
                } else {
                    notify("Loading example failed :(", "error")
                }
              },
              error: function(jqXHR, textStatus, errorThrown) {
                notify("Loading example failed :(", "error")
              }
            });
        }
    }

    $("#example-loader").change(loadExample);

    setTimeout(onCodeUpdate, timeWindow+50)

    var editorSession = editor.getSession();

    editor.commands.addCommand({
        name: 'save',
        bindKey: {win: 'Ctrl-S',  mac: 'Command-S'},
        exec: function(editor) {
            recompile()
        },
        readOnly: true
    });

    editor.commands.addCommand({
        name: 'verify',
        bindKey: {win: 'Alt-V',  mac: 'Alt-V'},
        exec: function(editor) {
            verifyCurrentFun()
        },
        readOnly: true
    });

    editorSession.on('change', function(e) {
        lastChange = new Date().getTime();
        setTimeout(onCodeUpdate, timeWindow+50)
    });

    function resizeEditor() {
        var heightRest = 100
        var widthRest = 450

        $('#codecolumn')
            .height($(window).height()-heightRest)
            .width($(window).width()-widthRest);

        $('#leoninput')
            .height($(window).height()-heightRest)
            .width($(window).width()-widthRest);

        $('#codebox')
            .height($(window).height()-heightRest)
            .width($(window).width()-widthRest);

        editor.resize();
    };

    $(window).resize(resizeEditor);

    resizeEditor();

    var chooseRulesDisplayer = null

    handlers["synthesis_choose_rules"] = function(data) {
        if (chooseRulesDisplayer != null) {
            chooseRulesDisplayer(data.cid, data.rulesApps)
        } else {
            error("I don't know how to display this..");
        }
    }

    handlers["synthesis_replace_code"] = function(data) {
        editorSession.setValue(data.newCode)
        recompile()
    }

    var currentMousePos = { x: -1, y: -1 };

    $(document).mousemove(function(event) {
        currentMousePos = { x: event.pageX, y: event.pageY };
    });

    editorSession.selection.on('changeCursor', function(e) {
        var cursor = editorSession.selection.getCursor()

        var token     = editorSession.getTokenAt(cursor.row, cursor.column);

        $("#editorMenu").hide();

        if (token != null) {
            var screenX = currentMousePos.x
            var screenY = currentMousePos.y

            var prevToken = editorSession.getTokenAt(cursor.row, token.start-1);

            if (token.type == "identifier.choose" && token.value == "choose" && context == "synthesis") {

                if (compilationStatus == 1) {
                    var msg = JSON.stringify({
                        action: "synthesis_getRulesToApply",
                        chooseLine: cursor.row+1,
                        chooseColumn: token.start+1,
                    })

                    chooseRulesDisplayer = function(cid, rulesApp) {
                        return synthesisDisplayMenu(screenX, screenY, cid, rulesApp);
                    }

                    leonSocket.send(msg)
                } else {
                    synthesisDisplayMenu(screenX, screenY, 0, []);
                }
            } else if (token.type == "identifier" && prevToken != null && prevToken.type == "keyword" && prevToken.value == "def" && context == "verification") {
                var found = false;
                var annots = editorSession.getAnnotations();
                for (var i = 0; i < annots.length && !found; i++) {
                    if (annots[i].row == cursor.row) {
                        found = true;
                    }
                }
                if (found) {
                    verificationDisplayMenu(screenX, screenY, token.value);
                }
                
            }
        }
    });

    function verificationDisplayMenu(screenX, screenY, fname) {
        $("#editorMenu").html("");

        if (compilationStatus == 1) {
            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-circle-triangle-s"></span>Automated Verification:</a></li>');

            $("#editorMenu").append('<li action="verify" fname="'+fname+'"><a href="#">Verify</a></li>');
        } else if (compilationStatus == 0) {
            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-alert"></span>Not yet compiled...</a></li>');
        } else {
            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-alert"></span>Compilation failed!</a></li>');
        }

        $("#editorMenu").menu({
            select: function( event, ui) {
                var r = ui.item;

                if (r.attr("action") == "verify") {
                    verifyFun(r.attr("fname"))
                }

                $("#editorMenu").hide();
                event.preventDefault();
            }
        });

        $("#editorMenu").show();
        $("#editorMenu").menu("refresh");
        $("#editorMenu").css({
            "position": "absolute",
            "top": screenY+"px",
            "left": screenX+"px",
            "z-index": 100
        });
    }

    function verifyCurrentFun() {
        var cursor = editorSession.selection.getCursor()

        var res = editor.find('def', {
            backwards: true,
            wholeWord: true
        });

        editorSession.selection.moveCursorToPosition(cursor)
        editorSession.selection.clearSelection();

        var found = false;
        var annots = editorSession.getAnnotations();
        for (var i = 0; i < annots.length && !found; i++) {
            if (annots[i].row == res.end.row) {
                found = true;
            }
        }

        if (found) {
            var tokens    = editorSession.getTokens(res.end.row)
            var pastDef   = false
            var fname     = null;
            for (var i = 0; i < tokens.length && fname == null; i++) {
                if (tokens[i].type == "keyword" && tokens[i].value == "def") {
                    pastDef = true
                } else if(pastDef && tokens[i].type == "identifier") {
                    fname = tokens[i].value
                }
            }

            if (fname != null) {
                verifyFun(fname)
            }
        }
    }

    function verifyFun(fname) {
        verificationFinished = false

        var msg = JSON.stringify(
          {action: "verification_doVerify", fname: fname}
        )

        leonSocket.send(msg)

        var pb = $("#verifyProgress")
        var pbl = $("#verifyProgress .progress-label")
        var pbv = pb.find(".ui-progressbar-value")

        pbl.text("Verifying...");

        pb.progressbar({
            value: false,
            complete: function() {
                pbl.text("Complete!");
            }
        });

        pbv.removeClass("failure").removeClass("success")
        $("#verifyResults").hide();

        $("#verifyDialog").dialog({
            modal: true,
            width: 500,
            buttons: {
                Cancel: function() {
                    $(this).dialog("close");
                }
            },
            close: function() {
                if (!verificationFinished) {
                    var msg = JSON.stringify(
                      {action: "verification_doCancel", fname: fname}
                    )

                    leonSocket.send(msg)
                    verificationFinished = true;
                }
            }
        });
    }

    function synthesisDisplayMenu(screenX, screenY, cid, rulesApps) {
        $("#editorMenu").html("");

        if (compilationStatus == 1) {
            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-circle-triangle-s"></span>Automated Search:</a></li>');

            $("#editorMenu").append('<li'+clazz+' action="search" cid="'+cid+'"><a href="#">Search</a></li>');

            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-circle-triangle-s"></span>Apply Rule:</a></li>');

            for (var i = 0; i < rulesApps.length; i++) {
                var app = rulesApps[i];
                var statusIcon = ""
                var clazz = ""

                if (app.status == "closed") {
                    statusIcon = '<span class="ui-icon ui-icon-alert"></span>'
                    clazz = ' class="ui-state-disabled"'
                }
                $("#editorMenu").append('<li'+clazz+' action="rule" cid="'+cid+'" rid="'+app.id+'"><a href="#">'+statusIcon+app.name+'</a></li>');
            }
        } else if (compilationStatus == 0) {
            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-alert"></span>Not yet compiled...</a></li>');
        } else {
            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-alert"></span>Compilation failed!</a></li>');
        }

        $("#editorMenu").menu({
            select: function( event, ui) {
                var r = ui.item;

                if (r.attr("action") == "rule") {
                    var msg = JSON.stringify(
                      {action: "synthesis_doApplyRule", cid: 1*r.attr("cid"), rid: 1*r.attr("rid")}
                    )

                    leonSocket.send(msg)
                } else if (r.attr("action") == "search") {
                    var cid = 1*r.attr("cid")

                    searchFinished = false

                    var msg = JSON.stringify(
                      {action: "synthesis_doSearch", cid: cid}
                    )

                    leonSocket.send(msg)

                    var pb = $("#searchProgress")
                    var pbl = $("#searchProgress .progress-label")

                    pb.progressbar({
                        value: false,
                        complete: function() {
                            pbl.text("Complete!");
                        }
                    });

                    $("#searchDialog").dialog({
                        modal: true,
                        width: 500,
                        buttons: {
                            Cancel: function() {
                                $(this).dialog("close");
                            }
                        },
                        close: function() {
                            if (!searchFinished) {
                                var msg = JSON.stringify(
                                  {action: "synthesis_doCancelSearch", cid: cid}
                                )

                                leonSocket.send(msg)
                                searchFinished = true;
                            }
                        }
                    });
                }

                $("#editorMenu").hide();
                event.preventDefault();
            }
        });

        $("#editorMenu").show();
        $("#editorMenu").menu("refresh");
        $("#editorMenu").css({
            "position": "absolute",
            "top": screenY+"px",
            "left": screenX+"px",
            "z-index": 100

        });
    }

    var storedCode = localStorage.getItem("leonEditorCode")
    if (storedCode != null) {
        editor.setValue(storedCode);
        editor.selection.clearSelection();
        editor.gotoLine(0);
    }
});
