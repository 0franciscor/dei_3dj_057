using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Shared;
using Moq;

namespace WarehouseManagementTest.Domain.Deliveries
{
    public class DeliveryTest
    {
        private string? id;

        private DateTime deliveryDate;

        private float loadTime;

        private float unloadTime;

        private string? destination;

        private float deliveryMass;

        [SetUp]
        public void Setup()
        {
            id = "testID";
            deliveryDate = new DateTime(2020, 12, 12);
            loadTime = 10;
            unloadTime = 20;
            destination = "testDestination";
            deliveryMass = 30;

        }

        [Test]
        public void DefineDriverServiceConstrutor()
        {
            var mockDeliveryRepo = new Mock<IDeliveryRepository>();
            var mockUnitWork = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnitWork.Object, mockDeliveryRepo.Object);

            Assert.That(service, Is.Not.Null);
        }
        [Test]
        public async Task CreateTest()
        {
            var deliveryExpected = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));

            var mockUnit = new Mock<IUnitOfWork>();
            mockUnit.Setup(repo => repo.CommitAsync());

            var mockDelivery = new Mock<IDeliveryRepository>();
            mockDelivery.Setup(repo => repo.Add(deliveryExpected));

            var service = new DeliveryService(mockUnit.Object, mockDelivery.Object);


            var deliveryDto = new DeliveryDTO
            {
                deliveryID = this.id,
                deliveryDate = this.deliveryDate,
                loadTime = this.loadTime,
                unloadTime = this.unloadTime,
                destination = this.destination,
                deliveryMass = this.deliveryMass
            };

            var deliveryResult = await service.CreateDelivery(deliveryDto);
            Assert.Multiple(() =>
                {
                    Assert.That(deliveryResult.deliveryID, Is.EqualTo(deliveryExpected.Id));
                    Assert.That(deliveryResult.deliveryDate, Is.EqualTo(deliveryExpected.deliveryDate.date));
                    Assert.That(deliveryResult.loadTime, Is.EqualTo(deliveryExpected.loadTime.time));
                    Assert.That(deliveryResult.unloadTime, Is.EqualTo(deliveryExpected.unloadTime.time));
                    Assert.That(deliveryResult.destination, Is.EqualTo(deliveryExpected.destination.destination));
                    Assert.That(deliveryResult.deliveryMass, Is.EqualTo(deliveryExpected.deliveryMass.mass));

                });
        }

        


    }
}