using EletricGo.Domain.Deliveries;
using EletricGo.Infrastructure.Shared;

namespace EletricGo.Infrastructure.Deliveries
{
    public class DeliveryRepository : BaseRepository<Delivery, DeliveryID>, IDeliveryRepository
    {
        public DeliveryRepository(EletricGoDBContext context) : base(context.Delivery)
        {

        }
    }

}