using EletricGo.Domain.Shared;


namespace EletricGo.Domain.Deliveries
{
    public class DeliveryDate : ValueObject
    {
        private DateTime Date { get; }
        
        public DeliveryDate(DateTime date)
        {
            Date = date;
        }
        
        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return Date;
        }
    }
   
}