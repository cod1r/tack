/* Type your code here, or load an example. */
#include <Foundation/Foundation.h>
#include <AppKit/AppKit.h>

void setup_macos_menu_bar() {
    NSApplication *app = [NSApplication sharedApplication];
    NSMenu *menu = app.mainMenu;
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:@"Menu" action:NULL keyEquivalent:@""];
    NSMenu *submenu = [[NSMenu alloc] initWithTitle:@"File"];
    submenu.minimumWidth = 200;
    NSMenuItem *save = [[NSMenuItem alloc] initWithTitle:@"Save" action:NULL keyEquivalent:@""];
    [submenu addItem:save];
    item.submenu = submenu;
    [menu insertItem:item atIndex:1];
}
