# FF Data Visualization
Visualizing data set found on MLIT (Ministry of Land, Infrastructure, Transport and Tourism Japan) 

** Practice using amazing R package called Circlize! (https://github.com/jokergoo/circlize)

#### Chord Diagram using 2016 Nationality OD Dataset. 

![](Output/2016_Nationality_OD_Multiples.png)

#### Chord Diagram using 2016 Transport OD Dataset. 

![](Output/2016_Transport_OD_Multiples.png)

### Prefecture Colouring....

For the above chart, I currently used below colouring for each prefecture
![](Images/JapanPrefectureColouring.png)



### FF Data (Flow of Foreigners in Japan Data Set)
Detail of Data Set can be found below (Site is written in Japanese)
http://www.mlit.go.jp/sogoseisaku/soukou/sogoseisaku_soukou_fr_000022.html


The site hosted 9 data sets at the time of writing.
OD suffix stands for "Origin Destination". 

- Nationality_OD files has matrix, with visitors (or foreginers) of Japan moving from one prefecture to another. 
- Transport_OD files has matrix, with visitors (or foreginers) of Japan moving from one prefecture to another with different transportation method.


|original file url                           |file.name               |
|:-------------------------------------------|:-----------------------|
|http://www.mlit.go.jp/common/001203832.xlsx |2014_Nationality_OD.xlsx |
|http://www.mlit.go.jp/common/001203840.xlsx |2015_Nationality_OD.xlsx |
|http://www.mlit.go.jp/common/001203846.xlsx |2016_Nationality_OD.xlsx |
|http://www.mlit.go.jp/common/001203834.xlsx |2014_Transport_OD.xlsx   |
|http://www.mlit.go.jp/common/001203842.xlsx |2015_Transport_OD.xlsx   |
|http://www.mlit.go.jp/common/001203848.xlsx |2016_Transport_OD.xlsx   |
|http://www.mlit.go.jp/common/001203831.xlsx |2014_Basic_DB.xlsx       |
|http://www.mlit.go.jp/common/001203839.xlsx |2015_Basic_DB.xlsx       |
|http://www.mlit.go.jp/common/001203844.xlsx |2016_Basic_DB.xlsx       |


# ChordDiagram of Travellers by different Transportation Method.
![example](https://github.com/chichacha/FF_Data_Visualization/blob/master/Output/2016_Transport_OD_Multiples.png?raw=true)
