package net.liftweb.treeview.model

object Role extends Role with MetaMegaTreeItem[Role] {
   
   override def dbTableName = "Role"
 
}

class Role extends MegaTreeItem[Role]{
  def getSingleton = Role
  def owner = Role
  
}