<script type="text/javascript">
 $(document).ready(function() {

   $("[data-toggle~=tooltip]").tooltip({ container: "body" });

   $("[data-toggle~=confirmation]").confirmation({
     container: "body",
     popover : true,
     singleton : true,
     placement : "bottom",
     title : "{t}activity.monitor.btn.delete.confirm{/t}",
     btnOkLabel : "{t}core.yes{/t}",
     btnCancelLabel : "{t}core.no{/t}",
     onConfirm : function(p_event, p_elem) {
       var l_form = $(p_elem).parents("form");
       l_form.attr("action", $(p_elem).attr("formaction"));
       l_form.submit();
     }
   });

   $("#activity").wapptable({
     bForceColFilter : true,
     bCookie : true,
     sEmptyCellFilterLabel : "{t}activity.monitor.status.valid{/t}",
     aaSorting : [[ 3, "desc" ]]
   });


   $("#image").hide();
   $("#image-alert").hide();

   $("#activity tbody").on("click", "tr", function() {
     var l_id = $(this).data("aid") || null;

     if (null == l_id)
     {
       $("#image").hide();
       $("#image-alert").fadeIn(200);
       return;
     }

     $("#image-alert").hide();

     $("#image").fadeOut({
       duration : 200,
       always : function() {
         $("#image").attr("src", "/activity/viewplate.php?id=" + l_id);
         $("#image").fadeIn(800);
       }});
   });

   $("#activity tbody tr:first-child").click();

   if ("true" == $.cookie("autoreload"))
   $("#autoreload").prop("checked", true);

   $("#autoreload").click(function() {
     $.cookie("autoreload", $(this).is(":checked"));
   });

   var g_maxID = {$maxid};
   function pollData()
   {
     $.getJSON("/activity/pending.php").done(function(p_data) {
       var l_id = p_data["maxid"];
       if (l_id > g_maxID && g_maxID >=0)
       {
         if (true == $("#autoreload").is(":checked"))
         location.reload();
         else
         {
           g_maxID = l_id;
           $("#dialog").modal("show");
         }
       }
     }).always(function() {
       setTimeout(pollData, 10000);
     });
   };
   pollData();
 });
</script>

<div class="container-fluid">
  <div class="row">
    <ol class="breadcrumb">
      <li><a href="/">{t}core.menu.home{/t}</a></li>
      <li><a href="/activity/">{t}activity.menu.title{/t}</a></li>
        {if $maxid <0 }
        <li><a href='/client/?action=view&id={$client->id}' >{$client->name}</a></li>
        <li class="active">{t}activity.client.movements_history{/t}</li>
        {else}
      <li class="active">{t}activity.menu.monitor{/t} &nbsp;&nbsp;
        <a href="/activity/?action=add" class="btn btn-sm btn-success glyphicon glyphicon-pencil" data-toggle="tooltip" data-placement="right" data-title="{t}activity.monitor.btn.add{/t}"></a>
        &nbsp;&nbsp;{t}activity.monitor.autoreload{/t} <input id="autoreload" type="checkbox"/>
      </li>
    {/if}
    </ol>
  </div> <!-- row -->


  <div class="row">
    <div class="col-lg-8">
      <table cellpadding="0" cellspacing="0" border="0" class="table table-bordered table-hover table-condensed table-responsive" id="activity">
        <thead>
          <tr>
            <th class="wp-filter"></th>
            <th class="text-center wp-filter">{t}activity.monitor.vehicletype{/t}</th>
            <th class="text-center wp-filter">{t}activity.monitor.camid{/t}</th>
            <th class="text-center wp-search">{t}core.date{/t}</th>
            <th class="text-center wp-filter">{t}activity.monitor.move{/t}</th>
            <th class="text-center wp-search">{t}activity.monitor.plate{/t}</th>
            <th class="text-center wp-search">{t}activity.monitor.confidence{/t}</th>
            <th class="text-center wp-search">{t}activity.monitor.account{/t}</th>
          </tr>
        </thead>
        <tbody>
          {foreach $activity as $c_act}
            <tr {if null != $c_act.image} data-aid="{$c_act.id}" {/if}>
              <td  style="min-width:135px;" {if 0 == $c_act.dismissed} class="text-right" {else} class="text-center" {/if} >
                {if 0 == $c_act.dismissed}
                  <form method="POST" action="/activity/">
                    <input type="hidden" name="aid" value="{$c_act.id}"/>
                    <div class="btn-group">
                      {if ($c_act.move == "IN") && (false == isset($c_act.account)) }
                        <button formaction="/activity/?action=reprint"  class="btn btn-primary btn-sm glyphicon glyphicon-print"         data-toggle="tooltip" data-placement="top" title="{t}activity.monitor.btn.print{/t}"/>
                        <button formaction="/activity/?action=checkout" class="btn btn-success btn-sm glyphicon glyphicon-shopping-cart" data-toggle="tooltip" data-placement="top" title="{t}activity.monitor.btn.checkout{/t}"/>
                      {/if}
                      <button formaction="/activity/?action=edit" class="btn btn-warning btn-sm glyphicon glyphicon-pencil" data-toggle="tooltip" data-placement="top" title="{t}activity.monitor.btn.edit{/t}"/>
                      <button formaction="/activity/?action=delete" class="btn btn-danger btn-sm glyphicon glyphicon-trash" data-toggle="tooltip confirmation" data-placement="top" data-title="{t}activity.monitor.btn.delete{/t}"/>
                    </div>
                  </form>
                {else}
                  <span class="text-danger">{t}activity.monitor.status.void{/t}</span>
                {/if}
              </td>
              <td class="text-center">{$c_act.vehicletype->name}</td>
              <td class="text-center">{$c_act.camid}</td>
              <td class="text-center">{$c_act.date}</td>
              <td class="text-center">
                {if $c_act.move == "IN"}
                  <span class="text-success">{t}activity.monitor.move.in{/t}</span>
                {else}
                  <span class="text-danger">{t}activity.monitor.move.out{/t}</span>
                {/if}
              </td>
              <td class="text-center">
                {assign classes "text-danger"}
                {if isset($c_act.account)}
                  {assign classes "text-primary"}
                {/if}

                <span class="{$classes}">{plate var="{$c_act.plate}"}</span>
              </td>
              <td class="text-center">{$c_act.confidence}%</td>
              <td class="text-center">
                {if isset($c_act.account)}
                  <a href="/client/?action=view&id={$c_act.account.id}">{$c_act.account.name}</a>
                {/if}
              </td>
            </tr>
          {/foreach}
        </tbody>
      </table>
    </div> <!-- col 8 -->
    <div class="col-lg-4">
      <img id="image" src="" class="img-responsive img-rounded"/>
      <div id="image-alert" class="container-fluid text-center alert alert-warning">
        {t}activity.monitor.noimage{/t}
      </div>
    </div> <!-- col 4 -->
  </div>  <!-- row -->
</div>

<div style="position:absolute; top:25%" class="modal" id="dialog" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
  <div class="modal-dialog modal-sm">
    <div class="modal-content">
      <div class="modal-body text-center">
        {t}activity.monitor.confirm_message{/t}
      </div>
      <div class="modal-footer">
        <div class="row">
          <div class="col-xs-6 text-center">
            <button type="button" class="btn btn-primary" onclick="location.reload()">{t}activity.monitor.reload{/t}</button>
          </div>
          <div class="col-xs-6 text-center">
            <button type="button" class="btn btn-default" data-dismiss="modal">{t}core.close{/t}</button>
          </div>
        </div>
      </div>
    </div><!-- /.modal-content -->
  </div><!-- /.modal-dialog -->
</div><!-- /.modal -->
