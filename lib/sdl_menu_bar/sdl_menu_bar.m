#include <Foundation/Foundation.h>
#include <AppKit/AppKit.h>
#include <caml/callback.h>

/*
according to
https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/MenuList/Articles/EnablingMenuItems.html

"If the menu item’s target is set, then NSMenu first checks to see if that object implements the item’s action method. If it does not, then the item is disabled. If the target does implement the item’s action method, NSMenu first checks to see if that object implements validateMenuItem: or validateUserInterfaceItem: method. If it does not, then the menu item is enabled. If it does, then the enabled status of the menu item is determined by the return value of the method."

So I just make an interface/class that extends or inherits from NSMenuItem
and then implement the action function in that interface/class which
allows the menuitem to be enabled.
*/
@interface SaveMenuItem : NSMenuItem
    +(SaveMenuItem*)get_menu_item;
    +(void) save_function;
@end
@implementation SaveMenuItem
    +(void) save_function {
        caml_callback(*caml_named_value("save_function_from_ocaml"), Val_unit);
    }
    +(SaveMenuItem*) get_menu_item {
        SEL saveAction = @selector(save_function);
        SaveMenuItem* menuItem = [[SaveMenuItem alloc] initWithTitle:@"Save" action:saveAction keyEquivalent:@"s"];
        menuItem.target = self;
        menuItem.enabled = true;
        menuItem.keyEquivalentModifierMask = NSEventModifierFlagCommand;
        return menuItem;
    }
@end
void setup_macos_menu_bar() {
    NSApplication *app = [NSApplication sharedApplication];
    NSMenu *menu = app.mainMenu;
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:@"Menu" action:NULL keyEquivalent:@""];
    NSMenu *submenu = [[NSMenu alloc] initWithTitle:@"File"];
    submenu.minimumWidth = 200;
    SaveMenuItem *save = [SaveMenuItem get_menu_item];
    [submenu addItem:save];
    item.submenu = submenu;
    [menu insertItem:item atIndex:1];
}
