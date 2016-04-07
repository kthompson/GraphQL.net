using System;
using System.Collections.Generic;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;

namespace GraphQL
{
    public static class SchemaRegistry
    {
        private static readonly object SchemaLock = new object();
        private static readonly Dictionary<Type, ClassMap> ClassMaps = new Dictionary<Type, ClassMap>();

        public static ClassMap<TClass> Register<TClass>(Action<ClassMap<TClass>> mapper = null)
        {
            var classMap = new ClassMap<TClass>();
            if (mapper != null)
                mapper(classMap);

            lock (SchemaLock)
            {
                ClassMaps.Add(classMap.ClassType, classMap);
            }
            return classMap;
        }

        public static bool IsClassMapRegistered<TClass>() => IsClassMapRegistered(typeof (TClass));
        public static bool IsClassMapRegistered(Type type)
        {
            if (type == null) throw new ArgumentNullException(nameof(type));

            lock (SchemaLock)
            {
                return ClassMaps.ContainsKey(type);
            }
        }
    }
}
