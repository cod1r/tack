#include <Foundation/Foundation.h>
#include <AppKit/AppKit.h>

@interface SaveMenuItem : NSMenuItem
    +(SaveMenuItem*)get_menu_item;
    +(void) CHICKEN;
@end
@implementation SaveMenuItem
    +(void) CHICKEN {
        NSLog(@"URMOM IS A FATASS");
    }
    +(SaveMenuItem*) get_menu_item {
        SEL saveAction = @selector(CHICKEN);
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
