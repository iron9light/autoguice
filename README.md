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
    @com.google.inject.ImplementedBy(classOf[XImpl])
    trait X {
      def name: String

      val someValue: Int
    }

    private class XImpl @javax.inject.Inject()(val name: String, val someValue: Int) extends X
```

Nothing works now.

I nead help: https://groups.google.com/d/topic/scala-language/3rw_pSHehd8/discussion