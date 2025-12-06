/* Type your code here, or load an example. */
#include <stdio.h>
#include <Foundation/Foundation.h>
#include <AppKit/AppKit.h>
#include "sdl_menu_bar.h"

@implementation Chicken
- (void) hi {
    printf ("CHICKEN");
}
@end
void print_menu_bar_title() {
    NSApplication *app = [NSApplication sharedApplication];
    NSMenu *menu = [app mainMenu];
    NSString *title = [menu title];
    NSLog(@"title: %@\n", title);
    NSLog(@"length: %lu\n", [title length]);
}
/*
int main () {
    Chicken *c = [Chicken alloc];
    [c hi];
    NSApplication *app = [NSApplication sharedApplication];
    NSMenu *menu = [app mainMenu];
    bool visible = [NSMenu menuBarVisible];
    if (visible) { printf("HI THERE\n"); }
    {
    NSString *title = [menu title];
    NSLog(@"title: %@\n", title);
    NSLog(@"length: %lu\n", [title length]);
    menu.title = @"CHICKEN";
    }
    NSSize size = [menu size];
    NSLog(@"size: %f %f\n", size.width, size.height);
    while (true) {}
}
*/
