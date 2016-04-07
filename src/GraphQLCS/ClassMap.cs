using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

namespace GraphQL
{
    public class ClassMap<TClass> : ClassMap
    {
        /// <summary>
        /// Creates a member map for the Id property and adds it to the class map.
        /// </summary>
        /// <typeparam name="TMember">The member type.</typeparam>
        /// <param name="propertyLambda">A lambda expression specifying the Id property.</param>
        /// <returns>The member map.</returns>
        public MemberMap<TClass, TMember> MapProperty<TMember>(Expression<Func<TClass, TMember>> propertyLambda)
        {
            return MapMember(propertyLambda);
        }

        /// <summary>
        /// Creates a member map and adds it to the class map.
        /// </summary>
        /// <typeparam name="TMember">The member type.</typeparam>
        /// <param name="memberLambda">A lambda expression specifying the member.</param>
        /// <returns>The member map.</returns>
        public MemberMap<TClass, TMember> MapMember<TMember>(Expression<Func<TClass, TMember>> memberLambda)
        {
            var memberInfo = GetMemberInfoFromLambda(memberLambda);
            return MapMember<TClass, TMember>(memberInfo);
        }
        
        /// <summary>
        /// Creates a member map for a field and adds it to the class map.
        /// </summary>
        /// <typeparam name="TMember">The member type.</typeparam>
        /// <param name="fieldLambda">A lambda expression specifying the field.</param>
        /// <returns>The member map.</returns>
        public MemberMap MapField<TMember>(Expression<Func<TClass, TMember>> fieldLambda)
        {
            return MapMember(fieldLambda);
        }



        public void ResolveType(Func<TClass, Type> resolver)
        {
            throw new NotImplementedException();
        }


        /// <summary>
        /// Creates a member map for a member and adds it to the class map.
        /// </summary>
        /// <param name="memberInfo">The member info.</param>
        /// <returns>The member map (so method calls can be chained).</returns>
        public MemberMap<TClass, TMember> MapMember<TClass, TMember>(MemberInfo memberInfo)
        {
            if (memberInfo == null)
            {
                throw new ArgumentNullException(nameof(memberInfo));
            }
            if (!(memberInfo is FieldInfo) && !(memberInfo is PropertyInfo))
            {
                throw new ArgumentException("MemberInfo must be either a FieldInfo or a PropertyInfo.", nameof(memberInfo));
            }
            EnsureMemberInfoIsForThisClass(memberInfo);

            var memberMap = (MemberMap<TClass, TMember>)_declaredMemberMaps.Find(m => m.MemberInfo == memberInfo);
            if (memberMap == null)
            {
                memberMap = new MemberMap<TClass, TMember>(this, memberInfo);
                _declaredMemberMaps.Add(memberMap);
            }
            return memberMap;
        }


        private static string GetMemberNameFromLambda<TMember>(Expression<Func<TClass, TMember>> memberLambda)
        {
            return GetMemberInfoFromLambda(memberLambda).Name;
        }

        private static MemberInfo GetMemberInfoFromLambda<TMember>(Expression<Func<TClass, TMember>> memberLambda)
        {
            var body = memberLambda.Body;
            MemberExpression memberExpression;
            switch (body.NodeType)
            {
                case ExpressionType.MemberAccess:
                    memberExpression = (MemberExpression)body;
                    break;
                case ExpressionType.Convert:
                    var convertExpression = (UnaryExpression)body;
                    memberExpression = (MemberExpression)convertExpression.Operand;
                    break;
                default:
                    throw new ArgumentException("Invalid lambda expression", nameof(memberLambda));
            }
            var memberInfo = memberExpression.Member;
            switch (memberInfo.MemberType)
            {
                case MemberTypes.Field:
                    break;
                case MemberTypes.Property:
                    if (memberInfo.DeclaringType.IsInterface)
                    {
                        memberInfo = FindPropertyImplementation((PropertyInfo)memberInfo, typeof(TClass));
                    }
                    break;
                default:
                    memberInfo = null;
                    break;
            }
            if (memberInfo == null)
            {
                throw new ArgumentException("Invalid lambda expression", nameof(memberLambda));
            }
            return memberInfo;
        }


        private static PropertyInfo FindPropertyImplementation(PropertyInfo interfacePropertyInfo, Type actualType)
        {
            var interfaceType = interfacePropertyInfo.DeclaringType;

            // An interface map must be used because because there is no
            // other officially documented way to derive the explicitly
            // implemented property name.
            var interfaceMap = actualType.GetInterfaceMap(interfaceType);

            var interfacePropertyAccessors = interfacePropertyInfo.GetAccessors(true);

            var actualPropertyAccessors = interfacePropertyAccessors.Select(interfacePropertyAccessor =>
            {
                var index = Array.IndexOf<MethodInfo>(interfaceMap.InterfaceMethods, interfacePropertyAccessor);

                return interfaceMap.TargetMethods[index];
            });

            // Binding must be done by accessor methods because interface
            // maps only map accessor methods and do not map properties.
            return actualType
                .GetProperties(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
                .Single(propertyInfo =>
                {
                    // we are looking for a property that implements all the required accessors
                    var propertyAccessors = propertyInfo.GetAccessors(true);
                    return actualPropertyAccessors.All(x => propertyAccessors.Contains(x));
                });
        }


        public MemberMap<TClass, TMember> MapField<TMember>(string fieldName)
        {
            if (fieldName == null)
            {
                throw new ArgumentNullException(nameof(fieldName));
            }

            var fieldInfo = this.ClassType.GetField(fieldName, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.DeclaredOnly);
            if (fieldInfo == null)
            {
                var message = $"The class '{this.ClassType.FullName}' does not have a field named '{fieldName}'.";
                throw new InvalidOperationException(message);
            }
            return MapMember<TClass, TMember>(fieldInfo);
        }

        private void EnsureMemberInfoIsForThisClass(MemberInfo memberInfo)
        {
            if (memberInfo.DeclaringType != ClassType)
            {
                throw new ArgumentOutOfRangeException(nameof(memberInfo),
                    $"The memberInfo argument must be for class {ClassType.Name}, but was for class {memberInfo.DeclaringType.Name}.");
            }
        }

        internal ClassMap(ClassMap baseClassMap = null) 
            : base(typeof(TClass), baseClassMap)
        {
        }

        public void MapInterface<TInterfaceType>()
        {
            var abstractType = typeof(TInterfaceType);
            var concreteType = typeof(TClass);

            if(!abstractType.IsAssignableFrom(concreteType))
                throw new ArgumentException($"The interface {abstractType.FullName} is not assignable from {concreteType.FullName}");


            throw new NotImplementedException();
        }
    }

    public class ClassMap
    {
        private readonly List<MemberMap> _declaredMemberMaps; // only the members declared in this class

        public Type ClassType { get; }
        public ClassMap BaseClassMap { get; }

        internal ClassMap(Type classType, ClassMap baseClassMap = null)
        {
            _declaredMemberMaps = new List<MemberMap>();
            this.BaseClassMap = baseClassMap;
            this.ClassType = classType;
        }
    }
}