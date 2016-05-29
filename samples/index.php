<?php

require_once(dirname(__FILE__) . "/local.php");
require_once(__WAPPCORE_DIR__  . "/core/classes/handler.php");

/**
 * C'est un super test
 *
 * avec un super string de la mort qui tue
 *
 * @return
 */
class HomePage extends Handler
{
  public function __construct()
  {
    parent::__construct();
  }

  public function h_default()
  {
    return $this->redirect("/activity");
  }
}

$l_page = new HomePage();
$l_page->process();

?>
