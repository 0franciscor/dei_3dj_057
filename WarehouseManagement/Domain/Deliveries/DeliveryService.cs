using EletricGo.Domain.Shared;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace EletricGo.Domain.Deliveries
{
    public class DeliveryService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IDeliveryRepository _deliveryRepository;

        public DeliveryService(IUnitOfWork unitOfWork, IDeliveryRepository deliveryRepository)
        {
            _unitOfWork = unitOfWork;
            _deliveryRepository = deliveryRepository;
        }

        public async Task<List<DeliveryDTO>> getDeliveries()
        {
            var deliveries = await _deliveryRepository.GetAll();
            return deliveries.Select(x => x.toDeliveryDTO()).ToList();
        }

        public async Task<DeliveryDTO> getDelivery(DeliveryID id)
        {
            var delivery = await _deliveryRepository.GetByID(id);
            return delivery.toDeliveryDTO();
        }

        public async Task<DeliveryDTO> createDelivery(DeliveryDTO deliveryDTO)
        {
            var delivery = new Delivery(deliveryDTO);
            await _deliveryRepository.Add(delivery);
            try
            {
                await this._unitOfWork.CommitAsync(); // LINHA IMPORTANTE QUE TE ESQUECESTE 
            }
            catch (Exception exp)
            {
                // Log what you need from here.
                throw new InvalidOperationException("Data could not be read", exp);
            }
            
            return delivery.toDeliveryDTO();
        }

        public async Task<DeliveryDTO> updateDelivery(string id, DeliveryDTO deliveryDTO)
        {
            var delivery = await _deliveryRepository.GetByID(new DeliveryID(id));
            delivery.update(deliveryDTO);
            await _deliveryRepository.Update(delivery);
            return delivery.toDeliveryDTO();
        }

        public async Task<DeliveryDTO> deleteDelivery(string id)
        {
            var delivery = await _deliveryRepository.GetByID(new DeliveryID(id));
            _deliveryRepository.Delete(delivery);
            return delivery.toDeliveryDTO();
        }
    }

}