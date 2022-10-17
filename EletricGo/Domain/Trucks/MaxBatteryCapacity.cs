using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks
{
    public class MaxBatteryCapacity : ValueObject
    {
        private float MaxBatteryCapacityValue { get; }

        public MaxBatteryCapacity(float maxBatteryCapacityValue)
        {
            MaxBatteryCapacityValue = maxBatteryCapacityValue;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return MaxBatteryCapacityValue;
        }
    }    
}
