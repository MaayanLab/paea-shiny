// Change status of the action button depending on the message

Shiny.addCustomMessageHandler('button_status_message',
    function(message) {
        if(message.disable) {

            if(message.uncheck) {
                $(message.id).prop( 'disabled', 'true').addClass('disabled').removeProp('checked');
            } else {
                $(message.id).prop( 'disabled', 'true').addClass('disabled');
            }

        } else {
            $(message.id).removeProp('disabled').removeClass('disabled');
        }
    }
);   
