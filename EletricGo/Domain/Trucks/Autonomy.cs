using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks
{
    public class Autonomy : ValueObject
    {
        private float AutonomyValue { get; }

        public Autonomy(float autonomyValue)
        {
            AutonomyValue = autonomyValue;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return AutonomyValue;
        }
    }    
}