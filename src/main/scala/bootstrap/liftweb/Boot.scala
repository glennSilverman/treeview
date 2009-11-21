package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import _root_.net.liftweb.treeview.model._
import _root_.net.liftweb.widgets._
import tree._


/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?)
      DB.defineConnectionManager(DefaultConnectionIdentifier,
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
  Props.get("db.url") openOr "jdbc:h2:lift_proto.db",
Props.get("db.user"), Props.get("db.password")))
  
    
    /*if (!DB.jndiJdbcConnAvailable_?)
      DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    */

    // where to search snippet
    LiftRules.addToPackages("net.liftweb.treeview")
    Schemifier.schemify(true, Log.infoF _, User, Role)

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home")) ::
    Menu(Loc("Static", Link(List("static"), true, "/static/index"), "Static Content")) :: 
    User.sitemap ::: Role.menus

    LiftRules.setSiteMap(SiteMap(entries:_*))

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)
    
    LiftRules.dispatch.append {
    	case Req("api" :: "role" :: id :: Nil, _, _) => ()=>Role.editMe(id)
    }
    
    LiftRules.useXhtmlMimeType = false 
    
    TreeView.init 

    S.addAround(DB.buildLoanWrapper)
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }

}





