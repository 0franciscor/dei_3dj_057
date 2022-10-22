using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Trucks.ValueObjects
{
    public class Autonomy : ValueObject
    {
        private float autonomyValue;

        public Autonomy(float autonomyValue)
        {
            this.autonomyValue = autonomyValue;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return this.autonomyValue;
        }

        public float asFloat()
        {
            return this.autonomyValue;
        }


    }    
}