using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace GraphQL.Tests
{

    // translation of https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js



    //public interface IGraphQLQuery
    //{
    //    GraphQLSchema GetSchema();
    //}

    /// <summary>
    /// This is the type that will be the root of our query, and the
    /// entry point into our schema.It gives us the ability to fetch
    /// objects by their IDs, as well as to fetch the undisputed hero
    /// of the Star Wars trilogy, R2-D2, directly.
    /// </summary>
    public class StarwarsSchema //: IGraphQLQuery
    {
        public StarwarsSchema()
        {
            SchemaRegistry.Register<Episode>();

            SchemaRegistry.Register<ICharacter>(cm =>
            {
                cm.MapProperty(x => x.Id);
                cm.MapProperty(x => x.Name);
                cm.MapProperty(x => x.AppearsIn);
                cm.MapProperty(x => x.Friends);

                //TODO: Do we even need this? We should be able to determine type with built in type checking.
                cm.ResolveType(instance => GetHuman(instance.Id) != null ? typeof(Human) : typeof(Droid));
            });

            SchemaRegistry.Register<Human>(cm =>
            {
                cm.MapProperty(x => x.Id);
                cm.MapProperty(x => x.Name);
                cm.MapProperty(x => x.AppearsIn);
                cm.MapProperty(x => x.Friends).Resolve(instance => GetFriends(instance));
                cm.MapProperty(x => x.HomePlanet);

                // Do we need this?
                cm.MapInterface<ICharacter>();
            });

            SchemaRegistry.Register<Droid>(cm =>
            {
                cm.MapProperty(x => x.Id);
                cm.MapProperty(x => x.Name);
                cm.MapProperty(x => x.AppearsIn);
                cm.MapProperty(x => x.Friends).Resolve(instance => GetFriends(instance));
                cm.MapProperty(x => x.PrimaryFunction);
                cm.MapInterface<ICharacter>();
            });
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="episode">
        /// If omitted, returns the hero of the whole saga. 
        /// If provided, returns the hero of that particular episode.
        /// </param>
        /// <returns></returns>
        public ICharacter Hero(Episode episode)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="id">id of the human</param>
        /// <returns></returns>
        public Human Human(string id)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="id">id of the droid</param>
        /// <returns></returns>
        public Droid Droid(string id)
        {
            throw new NotImplementedException();
        }


        private ICharacter GetHuman(string id)
        {
            throw new NotImplementedException();
        }

        private List<ICharacter> GetFriends(Human human)
        {
            throw new NotImplementedException();
        }

        private List<ICharacter> GetFriends(Droid human)
        {
            throw new NotImplementedException();
        }
    }
}
