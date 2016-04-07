using System.Collections.Generic;

namespace GraphQL.Tests
{
    /// <summary>
    /// A humanoid creature in the Star Wars universe.
    /// </summary>
    public class Human : ICharacter
    {
        /// <summary>
        /// The id of the human.
        /// </summary>
        public string Id { get; set; }
        /// <summary>
        /// The name of the human.
        /// </summary>
        public string Name { get; set; }
        /// <summary>
        /// The friends of the human, or an empty list if they have none.
        /// </summary>
        public List<ICharacter> Friends { get; set; }
        /// <summary>
        /// Which movies they appear in.
        /// </summary>
        public List<Episode> AppearsIn { get; set; }

        /// <summary>
        /// The home planet of the human, or null if unknown.
        /// </summary>
        public string HomePlanet { get; set; }
    }
}