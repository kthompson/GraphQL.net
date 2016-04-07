using System;
using System.Reflection;

namespace GraphQL
{
    public class MemberMap
    {
        public MemberInfo MemberInfo { get; }
        public string MemberName => this.MemberInfo.Name;
        public Type MemberType { get; }
        public ClassMap ClassMap { get; }
        public bool CanBeNull { get; private set; }

        internal MemberMap(ClassMap classMap, MemberInfo memberInfo)
        {
            this.ClassMap = classMap;
            this.MemberInfo = memberInfo;
            this.MemberType = GetMemberInfoType(memberInfo);
        }

        public MemberMap SetCanBeNull(bool value)
        {
            this.CanBeNull = value;
            return this;
        }

        static Type GetMemberInfoType(MemberInfo memberInfo)
        {
            if (memberInfo == null)
                throw new ArgumentNullException(nameof(memberInfo));

            if (memberInfo.MemberType == MemberTypes.Field)
                return ((FieldInfo) memberInfo).FieldType;

            if (memberInfo.MemberType == MemberTypes.Property)
                return ((PropertyInfo) memberInfo).PropertyType;

            throw new NotSupportedException("Only field and properties are supported at this time.");
        }
    }

    public class MemberMap<TClass, T> : MemberMap
    {
        internal MemberMap(ClassMap<TClass> classMap, MemberInfo memberInfo) 
            : base(classMap, memberInfo)
        {
        }

        public void Resolve<TResult>(Func<TClass, TResult> func)
        {
            throw new NotImplementedException();
        }
    }
}