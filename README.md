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

## Why do I create this plugin?

I tried a lot of DI pattern and frameworks: cake pattern, guice, subcut and more. see [cake_guice_subcut](https://github.com/iron9light/cake_guice_subcut)
No one is good enough, I think.

## The benefit of this plugin

 * Just write the meanful logic. No more boilerplate code.

 * Easy to change your DI solutions. This plugin in will support more DI framewok in the future.

## About the name: autoguice

I wanna name it **autoinjection**, but it can only inject guice code now.

I choose guice for the first try, since it's stable and easy to implement. And it taste nice.

I wish it can become **autoinjection** one day.