# A simple scala compiler plugin.

This compiler plugin will generate implement class for trait/abstract class like this:

```scala
    @autoinject
    trait X {
      def name: String

      val someValue: Int
    }
```

The generated code:

```scala
    @autoinject
    @com.google.inject.ImplementedBy(classOf[X__Impl])
    trait X {
      def name: String

      val someValue: Int
    }

    private class X__Impl @javax.inject.Inject()(val name: String, val someValue: Int) extends X
```

It works, baby :-P