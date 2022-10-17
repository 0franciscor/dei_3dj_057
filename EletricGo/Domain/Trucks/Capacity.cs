using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks
{
    public class Capacity : IValueObject<Capacity>
    {
        private float capacity;

        public Capacity(float capacity)
        {
            this.capacity = capacity;
        }

        public String toString()
        {
            return capacity.ToString();
        }
        
        public Boolean equals(Object obj)
        {
            return true;
        }
        
        public int hashCode()
        {
            return 0;
        }

        public float AsFloat()
        {
            return capacity;
        }

    }   
} 
