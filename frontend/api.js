
function getopen(onSuccess, onError)
{
  $.ajax(
    { url: '/open'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
