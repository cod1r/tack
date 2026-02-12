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
@interface FileMenuItem : NSMenuItem
    +(void) save_function;
    +(FileMenuItem*)get_save_menu_item;

    +(void) copy_function;
    +(FileMenuItem*)get_copy_menu_item;

    +(void) paste_function;
    +(FileMenuItem*)get_paste_menu_item;

    +(void) cut_function;
    +(FileMenuItem*)get_cut_menu_item;

    +(void) undo_function;
    +(FileMenuItem*)get_undo_menu_item;
@end
@implementation FileMenuItem
    +(void) save_function {
        caml_callback(*caml_named_value("save_function_from_ocaml"), Val_unit);
    }
    +(FileMenuItem*) get_save_menu_item {
        SEL saveAction = @selector(save_function);
        FileMenuItem* menuItem = [[FileMenuItem alloc] initWithTitle:@"Save" action:saveAction keyEquivalent:@"s"];
        menuItem.target = self;
        menuItem.enabled = true;
        menuItem.keyEquivalentModifierMask = NSEventModifierFlagCommand;
        return menuItem;
    }

    +(void) copy_function {
        caml_callback(*caml_named_value("copy_function_from_ocaml"), Val_unit);
    }
    +(FileMenuItem*) get_copy_menu_item {
        SEL copyAction = @selector(copy_function);
        FileMenuItem* menuItem = [[FileMenuItem alloc] initWithTitle:@"Copy" action:copyAction keyEquivalent:@"c"];
        menuItem.target = self;
        menuItem.enabled = true;
        menuItem.keyEquivalentModifierMask = NSEventModifierFlagCommand;
        return menuItem;
    }

    +(void) paste_function {
        caml_callback(*caml_named_value("paste_function_from_ocaml"), Val_unit);
    }
    +(FileMenuItem*) get_paste_menu_item {
        SEL pasteAction = @selector(paste_function);
        FileMenuItem* menuItem = [[FileMenuItem alloc] initWithTitle:@"Paste" action:pasteAction keyEquivalent:@"v"];
        menuItem.target = self;
        menuItem.enabled = true;
        menuItem.keyEquivalentModifierMask = NSEventModifierFlagCommand;
        return menuItem;
    }

    +(void) cut_function {
        caml_callback(*caml_named_value("cut_function_from_ocaml"), Val_unit);
    }
    +(FileMenuItem*) get_cut_menu_item {
        SEL cutAction = @selector(cut_function);
        FileMenuItem* menuItem = [[FileMenuItem alloc] initWithTitle:@"Cut" action:cutAction keyEquivalent:@"x"];
        menuItem.target = self;
        menuItem.enabled = true;
        menuItem.keyEquivalentModifierMask = NSEventModifierFlagCommand;
        return menuItem;
    }

    +(void) undo_function {
        caml_callback(*caml_named_value("undo_function_from_ocaml"), Val_unit);
    }
    +(FileMenuItem*) get_undo_menu_item {
        SEL undoAction = @selector(undo_function);
        FileMenuItem* menuItem = [[FileMenuItem alloc] initWithTitle:@"Undo" action:undoAction keyEquivalent:@"z"];
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
    FileMenuItem *save = [FileMenuItem get_save_menu_item];
    [submenu addItem:save];
    FileMenuItem *copy = [FileMenuItem get_copy_menu_item];
    [submenu addItem:copy];
    FileMenuItem *paste = [FileMenuItem get_paste_menu_item];
    [submenu addItem:paste];
    FileMenuItem *cut = [FileMenuItem get_cut_menu_item];
    [submenu addItem:cut];
    FileMenuItem *undo = [FileMenuItem get_undo_menu_item];
    [submenu addItem:undo];
    item.submenu = submenu;
    [menu insertItem:item atIndex:1];
}
