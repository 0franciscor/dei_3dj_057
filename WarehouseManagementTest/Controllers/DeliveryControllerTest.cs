using EletricGo.Controllers;
using EletricGo.Domain.Deliveries;
using Moq;
using EletricGo.Domain.Shared;

namespace DDDNetCoreTests.Controllers.Deliveries
{
    internal class DeliveryControllerTest
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

        //SETUP LISTS FOR GETALL()
        private List<DeliveryDTO> GetDeliveriesList()
        {
            var deliveryList = new List<DeliveryDTO>
            {
                new DeliveryDTO(){deliveryID = id, deliveryDate = this.deliveryDate, loadTime = this.loadTime, unloadTime = this.unloadTime, destination = this.destination, deliveryMass = this.deliveryMass},
                new DeliveryDTO(){deliveryID = "testID2", deliveryDate = this.deliveryDate, loadTime = this.loadTime, unloadTime = this.unloadTime, destination = this.destination, deliveryMass = this.deliveryMass},
                new DeliveryDTO(){deliveryID = "testID3", deliveryDate = this.deliveryDate, loadTime = this.loadTime, unloadTime = this.unloadTime, destination = this.destination, deliveryMass = this.deliveryMass},
                new DeliveryDTO(){deliveryID = "testID4", deliveryDate = this.deliveryDate, loadTime = this.loadTime, unloadTime = this.unloadTime, destination = this.destination, deliveryMass = this.deliveryMass},
                new DeliveryDTO(){deliveryID = "testID5", deliveryDate = this.deliveryDate, loadTime = this.loadTime, unloadTime = this.unloadTime, destination = this.destination, deliveryMass = this.deliveryMass},
            };

            return deliveryList;
        }

        [Test]
        public async Task GetAllTest()
        {
            var expectedList = GetDeliveriesList();

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();
            
            var serviceMock = new Mock<DeliveryService>(mockUnit.Object, mockRepository.Object);
            serviceMock.Setup(repo => repo.GetDeliveries()).ReturnsAsync(expectedList);

            var deliveryController = new DeliveryController(serviceMock.Object);
            var resultList = await deliveryController.GetAll();

            for(int i = 0; i < expectedList.Count; i++)
            {
                Assert.That(resultList.Value[i].deliveryID, Is.EqualTo(expectedList[i].deliveryID));
            }

        }
                
                
        

    }
}
