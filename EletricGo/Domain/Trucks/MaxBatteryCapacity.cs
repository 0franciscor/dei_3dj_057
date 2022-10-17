using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks
{
    public class MaxBatteryCapacity : IValueObject<MaxBatteryCapacity>
    {

        private float maxBatteryCapacity;

        public MaxBatteryCapacity(float maxBatteryCapacity)
        {
            this.maxBatteryCapacity = maxBatteryCapacity;
        }

        public String toString()
        {
            return maxBatteryCapacity.ToString();
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
            return maxBatteryCapacity;
        }

    }    
}
