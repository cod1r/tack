#include <Foundation/Foundation.h>
#include <AppKit/AppKit.h>
#include <caml/callback.h>

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
