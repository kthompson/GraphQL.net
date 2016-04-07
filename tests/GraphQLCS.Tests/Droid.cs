using System.Collections.Generic;

namespace GraphQL.Tests
{
    /// <summary>
    /// A mechanical creature in the Star Wars universe.
    /// </summary>
    public class Droid : ICharacter
    {
        /// <summary>
        /// The id of the droid.
        /// </summary>
        public string Id { get; set; }
        /// <summary>
        /// The name of the droid.
        /// </summary>
        public string Name { get; set; }
        /// <summary>
        /// The friends of the droid, or an empty list if they have none.
        /// </summary>
        public List<ICharacter> Friends { get; set; }
        /// <summary>
        /// Which movies they appear in.
        /// </summary>
        public List<Episode> AppearsIn { get; set; }

        /// <summary>
        /// The primary function of the droid.
        /// </summary>
        public string PrimaryFunction { get; set; }
    }
}