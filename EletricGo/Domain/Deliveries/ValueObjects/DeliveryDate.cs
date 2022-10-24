using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class DeliveryDate : ValueObject
    {
        private DateTime date { get; }
        
        public DeliveryDate(DateTime date)
        {
            this.date = date;
        }

        public DateTime AsDateTime()
        {
            return date;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return date;
        }
    }
   
}