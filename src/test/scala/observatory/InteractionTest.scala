package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

trait InteractionTest extends FunSuite with Checkers {
    
    test("tileLocation"){
      assert(Interaction.tileLocation(0, 0, 0) === Location(85.05112877980659, -180.0))
      assert(Interaction.tileLocation(1, 1, 1) === Location(0.0, 0.0))
      assert(Interaction.tileLocation(2, 1, 2) === Location(0.0, -90.0))
    }
    
}
