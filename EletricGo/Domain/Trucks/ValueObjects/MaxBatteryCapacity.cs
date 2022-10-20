using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks.ValueObjects
{
    public class MaxBatteryCapacity : ValueObject
    {
        private float maxBatteryCapacityValue;

        public MaxBatteryCapacity(float maxBatteryCapacityValue)
        {
            this.maxBatteryCapacityValue = maxBatteryCapacityValue;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return this.maxBatteryCapacityValue;
        }

        public float asFloat()
        {
            return maxBatteryCapacityValue;
        }
    }    
}
