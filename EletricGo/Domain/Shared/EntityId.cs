using System;

namespace EletricGo.Domain.Shared
{
    /// <summary>
    /// Base class for entities.
    /// </summary>
    public abstract class EntityID: IEquatable<EntityID>, IComparable<EntityID>
    {
        protected Object ObjValue {get;}

        public String Value { 
            get { 
                    if (this.ObjValue.GetType() == typeof(String))
                        return (String) this.ObjValue;
                    return AsString();
                } 
        }

        protected EntityID(Object value)
        {
            if (value.GetType() == typeof(String))
                this.ObjValue = createFromString((String)value);
            else
                this.ObjValue = value;
        }

       
        protected abstract Object createFromString(String text);
        
        public abstract String AsString();


        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            return obj is EntityID other && Equals(other);
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public bool Equals(EntityID other)
        {
            if (other == null)
                return false;
            if (this.GetType() != other.GetType())
                return false;
            return this.Value == other.Value;
        }

        public int CompareTo(EntityID other){
            if (other == null)
                return -1;
            return this.Value.CompareTo(other.Value);
        }

        public static bool operator ==(EntityID obj1, EntityID obj2)
        {
            if (object.Equals(obj1, null))
            {
                if (object.Equals(obj2, null))
                {
                    return true;
                }
                return false;
            }
            return obj1.Equals(obj2);
        }
        public static bool operator !=(EntityID x, EntityID y) 
        {
            return !(x == y);
        }
    }
   
}