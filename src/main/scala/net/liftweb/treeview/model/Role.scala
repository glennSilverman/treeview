package net.liftweb.treeview.model

import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.http._
import js._
import js.JsCmds._
import js.jquery._
import JE._
import JqJsCmds._
import JqJE._
import SHtml._
import java.util.Date
import _root_.scala.xml._
import Helpers._
import _root_.scala.collection.mutable._
import S._
import _root_.net.liftweb.widgets.tree._

object Role extends Role with KeyedMetaMapper[Long, Role] {
	
  def buildTreeViewMenu = 
	 Menu(Loc("TREE", "role" :: Nil, "Roles",
			      treeItems, treeSnippets))
  
  def treeEditViewMenu = Menu(Loc("EDIT_TREE", List("api", "role") -> true, "Edit Tree",Hidden))  
  
  def menus:List[Menu] = buildTreeViewMenu :: treeEditViewMenu :: Nil
  
  def treeItems = Template({ () =>
	  <lift:surround with="default" at="content">
	   <lift:roles.buildTree/>       
	    <h3>Roles</h3>        
       <div id="entryform"> 
          <div class="column span-5"> 
             <ul id="tree"/>
          </div>
          <div class="column span-5">
           <div id="role_edit"/>
          </div>
       </div>
      <hr />
      <lift:roles.addNew>
         <role:addNew/>                   
      </lift:roles.addNew>         
      <div id="role_create"/>
	  </lift:surround>
	})
  
  lazy val treeSnippets = new DispatchLocSnippets {
    val dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
      case "roles.buildTree" => buildTree
      case "roles.addNew" => addNew(create, S.??("Created"))
       
    }
  }
  
  def buildTree(in:NodeSeq):NodeSeq = {    
   
	   object MyJqLoad {
	     def apply(content: JsExp) = new JsExp with JQueryRight with JQueryLeft {
	       def toJsCmd = JqId("role_edit").toJsCmd + ".load(" + content.toJsCmd + JsRaw("""+this.id""") + ")"
	     }
	   }
	       
       val host = "http://" + S.hostName  
       val link = host + ":8080" + S.contextPath + "/api/role/"
   
       val func = AnonFunc( MyJqLoad(link))
       
       TreeView("tree", JsObj(("persist", "location"), ("toggle",  func)), loadTree, loadNode)
    }
  
  def loadTree() = 
    findAll.filter(e => e.parentId == -1).map(e => node(e))
  
  
  def loadNode(ids: String): List[Tree] = 
    ids match {
    case "none" => Nil
    case x => findAll.filter(e => e.parentId.toString equals x).map(e => 
     	node(e))
    }
  
  def node(r:Role) = {
    def anchor = r.name.is
    if(hasKids(r))
       Tree(anchor, r.id.toString, true) 
    else
       Tree(anchor, r.id.toString, false)
 }
  
  def hasKids(r:Role) = findAll.filter(e => e.parentId == r.id.is) match {
    case Nil => false
    case _  => true
  }
  
  def addNew(role: Role, noticeMsg: String)(xhtml:NodeSeq):NodeSeq = 
    bind("role", xhtml, 
	       	"addNew" -> {SHtml.a({ ()=> 
	       	  SetHtml("role_create", edit(role))},
              Text("Add Role") 
	       	  )}
	         )
  
  def edit(role:Role) =
    <form>{role.toForm(Full("Save"), { _.save })}</form>
    
  def editMe(id:String): Box[LiftResponse] = {
    println("Edit role = " + id)
    findAll(By(Role.id, Integer.parseInt(id))) match {
      case Nil => Empty
      case rs =>  Full(CreatedResponse(edit(rs.first), "text/xhtml" ))  
    }  	
   }        
 
}

class Role extends LongKeyedMapper[Role] with IdPK {
	
  def getSingleton = Role      
  
  // Role name
  object name extends MappedString(this, 50)
  
  object parent extends MappedLongForeignKey(this, Role){
    override def dbIndexed_? = true   
    override def validSelectValues: Box[List[(Long, String)]] = 
      Full(
        List[(Long, String)]((-1, "NA")) ::: Role.findAll.map( x => (x.id.is, x.name.is) ))
    
    override def displayName = "Parent:"
  }
  
  def parentName =
	  parent.obj.map(_.name.is) openOr "NA"
  
  def parentId:Long =
    parent.obj.map(_.id.is) openOr -1
  
  
}
