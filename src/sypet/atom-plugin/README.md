# How to use Plugin
After navigating to the directory with `package.json` , on a terminal, run
```
apm link
 ```
 
This will make this package visible to Atom and it should now be accessible under `Packages->test-atom-package` in Atom.

Simply select a block of code you want to run Sypet on (with the correct custom comments needed for synthesis) and the plugin will replace `\\#Sypet` with the synthesized code! 

Here is an example of a valid selection - 

```java
//# java.awt.geom
//# java.awt.geom.Rectangle2D
//# foo
public static void shear(java.awt.geom.Rectangle2D sypet_arg0, double sypet_arg1, double sypet_arg2) {
    //#Sypet
}

// # //
public static boolean foo() throws Throwable {
    java.awt.geom.Rectangle2D area = new java.awt.geom.Rectangle2D.Double(10, 20, 10, 10);
    java.awt.geom.Rectangle2D target = new java.awt.geom.Rectangle2D.Double(20, 24, 15, 14); java.awt.geom.Rectangle2D result = shear(area, 0.5, 0.4);
    return (target.equals(result));
}
// # //

```
