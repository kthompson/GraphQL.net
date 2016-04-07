using System.Collections.Generic;

namespace GraphQL.Tests
{
    /// <summary>
    /// A character in the Star Wars Trilogy
    /// </summary>
    public interface ICharacter
    {
        /// <summary>
        /// The id of the character.
        /// </summary>
        string Id { get; set; }
        /// <summary>
        /// The name of the character.
        /// </summary>
        string Name { get; set; }
        /// <summary>
        /// The friends of the character, or an empty list 
        /// if they have none.
        /// </summary>
        List<Episode> AppearsIn { get; set; }
        /// <summary>
        /// Which movies they appear in.
        /// </summary>
        List<ICharacter> Friends { get; set; }
    }
}