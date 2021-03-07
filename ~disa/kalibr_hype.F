
\ загрузка таблиц калибровок  первый столбец - частота,  "постоянно растущая"
\ нанные пишутся в  действительных числах !!!!!
\ промежуточные данные -  линейно интерполируются 


\ tabl_kalibr NEW "ИМЯ"  = создание новой таблицы
\ "кол-во строк"  "кол-во столбцов" "ИМЯ" LoadDatas:   = инициализация загрузки таблицы  "ИМЯ"
\ 
\  LoadData: х1 х2 ...  \ загрузка данных вариант 1
\ х1 х2 ...х4 LoadData   \ загрузка данных вариант 2 

\ "ИМЯ" SeeDatas   -посмотреть таблицу данных
\  n  TakeData 
\ freq	n "ИМЯ"  TakeData \ получить данные из столбца n  с частотой freq из таблицы "ИМЯ" 

\ -----------------------------------
\  в конце есть   "LoadDatas: " с "перевернутыми" значсениями для удобства загрузки значений из внешнего файла
\ "ИМЯ"  "кол-во строк"  "кол-во столбцов" LoadDatas:   = инициализация загрузки таблицы  "ИМЯ"
\ -----------------------------------
\ загрузка калибровки из файла
\ "кол-во столбцов" "имя файла" LoadFile
\  

 REQUIRE  N_S_PickUpWord ~disa\dopoln.f
 REQUIRE  F. lib\include\float2.f
 REQUIRE  HYPE ~day\hype3\hype3.f
 REQUIRE  dBuV->V  ~disa/algoritm.f


 
0 VALUE tekdata \ однопоточная часть! ??  только для загрузки 


CLASS tabl_kalibr

CELL DEFS  datas \ ссылка на данные
CELL DEFS num_datas  \ количество загруженных ячеек
CELL DEFS max_datas  \ всего ячеек
CELL DEFS num_datas_in_string   \ количество данных в  строке
CELL DEFS fUllErr   \ ошибка обращения к несуществуюшщему элементу
 
: deltaFreq ( freq1 freq2 - flag) \ сравнение 2х  чисел c дельной аналог F= с учетом погрешностей 
||   F: freq   ||
freq F! 
freq F@ F- FABS
freq F@ F/ 
1e-5 F>
;



\ для определения частот на границе переключений вводим еще одну частоту\
\ по которой будем определять эту границу
1 FLOATS DEFS  LastFreq    \ предыдущая частота 
1 FLOATS DEFS  LastFreq2    \ пред-предыдущая частота 
\ и слово для запомнинания 
: SaveFreq \ F. 
	 LastFreq F@ LastFreq2 F!
	 LastFreq F! 
;


: adr_data_in_number    ( string row -- adr-data )
	|| D:  N_write ||
DUP max_datas > IF DROP max_datas  -1 fUllErr ! ELSE 0 fUllErr ! THEN \ защита от переполнения
	N_write ! 
\ ." ->" DUP .  N_write @ . ." <-"
	num_datas_in_string @ FLOATS  * datas @ +  N_write @ FLOATS +   
;
 

: take_freq_in_number \ матрица  частот и антенных факторов
	\   num_datas_in_string  @ FLOATS * datas @ + SF@
 \ ." ->" DUP . 
 	0  adr_data_in_number  F@  
\ DROP 1e1
\ FDUP F. ." <-"
;

: MinMax \ определение    границ  простых калибровочных файлов и корректировка значения 
	\	<< F: freq -- F:freq1 D: flag >>
	|| F: freq D: flag ||
	freq F! 0 flag !
	 0 take_freq_in_number			freq F@	F> IF  0		take_freq_in_number  freq F! 1 flag ! THEN
	 num_datas @ 1 - take_freq_in_number	freq F@ F< IF  num_datas @ 1 -  take_freq_in_number  freq F! 1 flag ! THEN 
	freq F@ flag @
;


 
:  take_data_in_number  (  n_freq , n_write  -- )
	\  || D:  N_write ||
	\ N_write ! num_datas_in_string @ FLOATS  * datas @ +  N_write @ FLOATS +
	adr_data_in_number  F@ 
;


: FreqNum ( freq -- num) \ определение номера ячейки по частоте
	||   D: n F: freq   ||adr_data_in_number
	freq F!  0 n ! 
	num_datas @ \ количество точек
	0  
	DO
		I  take_freq_in_number  \ CR FDUP F.  ." " I .  n @ . 
		freq  F@ 1.00001E F* F<=  IF I  n !  THEN 
	LOOP
	n @
;

: NextFreq ( freq_old -- freq_new )
	||   D: n F: freq F: newfreq  ||
	freq F!  
	\ проверяем 2 повторения по частоте в таблице  калибровки	
	 freq  F@    FreqNum  n ! 
	n @ 1 - take_freq_in_number     \ FDUP F. freq  F@  F. CR
	freq  F@  deltaFreq 
	IF \ разные частоты
		 n @ 1 + take_freq_in_number    freq F!
	ELSE \ одинаковые частоты - требуется дополнительная проверка
		LastFreq F@  LastFreq2 F@ 
		\ ." last=" LastFreq F@  F. LastFreq2 F@ F. CR 
		 deltaFreq  \ 2 раза подали чатоту?
		IF \ разные 
			 n @ 1 + take_freq_in_number    freq F!
		THEN
	THEN
	freq F@
\   CR
;

 : PredFreq ( freq_old -- freq_new ) \ перебор вниз!
	FreqNum       1 - take_freq_in_number      CR
 ;

: TakeData \ ( freq  num  -- data )
	|| D:  num   D: n F: freq   ||
	num ! \ номер точки 
	freq F!  0 n !
	num_datas @ \ количество точек
	0  
	DO \ прошли по всей матрице
		I  take_freq_in_number    freq F@  F<  IF I  n !  THEN 
	LOOP \ нашли номер точки
	LastFreq   F@  freq F@ deltaFreq  \ разные частоты
	IF  
		n @  num_datas @ 1- = IF num_datas @  2 -  n ! THEN	
		\ линейная инретполяция
		  \ (у1-1024 у2)/(х1-х2)*х1 +у1
		  n @ 1+ num @ take_data_in_number  n @ num @ take_data_in_number  F-  \ (уk-уn)  
		  n @ 1+  take_freq_in_number  n @  take_freq_in_number  F-  \ (хk-хn)
		 F/  ( ***/***)
		 freq F@
		 n @  take_freq_in_number    \ (***) *х1
		  F- F* \ (***) *х1
		   \  n 1+ ПолучитьАнтенныйФакторПоНомеру    FSWAP F- \ (***) + у1
		  n @ num @ take_data_in_number     F+ 
		\ CR n @ . num_datas @ . num @ .  F.
	ELSE	
		n @ 2 + num @ take_data_in_number        
	THEN
 ;

: Add_num_datas \ добавить новый номер  строчки
." Add_num_datas=" num_datas @ . max_datas @ . 
	num_datas @ max_datas @ < \ не превышать макс кол-во строчек
	IF
		 num_datas @ 1 + num_datas   !
	THEN
;


 : LoadData 
	 num_datas @ 1  adr_data_in_number F!
	 num_datas @ 0  adr_data_in_number F!
\	 num_datas @ 1 + num_datas   !
	 Add_num_datas 
;

\ создаем табличку  указывая размер ее (строк, столбцов)
\ : :SetData  datas ! ;
 :  LoadDatas:   (   adr_data,   n_Data )
	 SELF TO tekdata 
	 num_datas_in_string !  \ запомнили кол-во столбцов
	 DUP  max_datas !	\ запомнили максимальное кол-во строчек!
	 0  num_datas ! 
	   ."  datas @ = "  datas @ . CR
	  datas @ IF 	 datas @ FREE 	DROP THEN \ удалить прошлые таблички 
	 num_datas_in_string @ FLOATS *  CELL + ALLOCATE THROW   datas !
	 
 
;

: AddMassive ( num ) \ расширение таблицы
|| D: newdatas D: addstring D: nbytes ||
\ num_datas_in_string 
addstring !
max_datas @ num_datas_in_string @ FLOATS *  CELL +  nbytes !	\ размер прошлой таблицы
addstring @ num_datas_in_string @ FLOATS *  CELL		\ размер добавляемой части
nbytes @ +  ALLOCATE THROW   newdatas !				\ создали новую табличку

datas @ newdatas @ nbytes @  CMOVE \ копировали данные из таблички
 
max_datas @ addstring @ max_datas  ! \ обновилиразмер таблички
;

: SwapString  ( n1 n2 ) \ поменять 2 строчки местами
|| D: n1 D: n2 D: len ||
n2 ! n1 !
num_datas_in_string @ FLOATS   len ! \ размер  
len @  0 > 
IF

 n1 @      0 adr_data_in_number max_datas @ 0 adr_data_in_number len  @ CMOVE
 n2 @      0 adr_data_in_number n1 @      0 adr_data_in_number len  @ CMOVE
 max_datas @ 0 adr_data_in_number n2 @      0 adr_data_in_number len  @ CMOVE
THEN 
;

: DelString \ удалить строчку с схлопыванием окружающих
|| D: del_num ||
del_num !
del_num @ num_datas @  < 
IF 
	num_datas @  del_num @ 
	DO
	 I I 1 + SwapString
	LOOP
ELSE
	num_datas @ 1 - num_datas !
THEN
num_datas @ 1 - num_datas !
;

: InsString \ добавить строчку, с раздвижением окружающих
;

: SeeDatas \ { adr \ --  }
	 \ || D: adr || adr !
	\ num_datas @  . num_datas_in_string @  . CR
	CR
	num_datas @  max_datas @ MIN  DUP 0 >
	IF
		0 
		DO
			I take_freq_in_number F.
			num_datas_in_string @ 1 
			DO
				J I take_data_in_number F. 
			LOOP
			CR
		LOOP
	ELSE DROP THEN 
;



 : LoadData: ( num -- )
	\ num_datas @ . num_datas_in_string @ .  ." |"
	 num_datas_in_string @  0 
	 DO   
		 NextWord  \ 2DUP TYPE SPACE
		  STR>S  S>FLOAT  DROP \ F.
		 num_datas @ I adr_data_in_number F!
	LOOP
	\ num_datas @ 1  adr_data_in_number F!
	\ num_datas @ 0  adr_data_in_number F!
	 num_datas @ 1 + num_datas   !
;

: dispose 
	datas @ FREE THROW  
;

: SaveData   \ s-filename \ сохранение  только цифровых данных
|| D: file ||

	outFileCreate file !
	num_datas @   ."  num_datas = " DUP . CR 
	0 
	DO ."  I = " I . CR 
		I take_freq_in_number  FDUP  file @ FtoFile    F.
		num_datas_in_string @ 1 
		DO
			J I take_data_in_number  FDUP file @ FtoFile  F.
		LOOP
	file @ CRtoFile 
	\		CR
	LOOP 
	file @ outFileClose 
;


: SaveDataSource  \ s-filename \ сохранение данных как исходные тексты с командами загрузки
|| D: file ||

	outFileCreate file !
	num_datas @ 10 + file @ DtoFile 
	num_datas_in_string @	file @ DtoFile 
	" LoadDatas: "	file @ StoFile 
	file @ CRtoFile 
	CR
	num_datas @ 
	0 
	DO
		" LoadData:  "	file @ StoFile 
		I take_freq_in_number  FDUP  file @ FtoFile    F.
		num_datas_in_string @ 1 
		DO
			J I take_data_in_number  FDUP file @ FtoFile  F.
		LOOP
	file @ CRtoFile 
	\		CR
	LOOP 
	file @ outFileClose 
;



: LoadFile ( row file --   )  
\ прочитать данные из файла  когда известно только количество столбцов .
         || D: row D: file D: sadr   BUFS256: buf D: n ||
	sadr ! \ имя файла
 	row !	 \ кол-во столбцов
	sadr @ STR@  ."  outFileOpen= "  TYPE ."  " 
	sadr @ STR@ R/O  OPEN-FILE-SHARED THROW \ OPEN-FILE \ CREATE-FILE 
	file !
\ считаем кол-во строк
	0 n !
	BEGIN
		buf ADDR 255 file @ READ-LINE THROW NIP  0 =
		n @ 1 + n !
	UNTIL
n @ 1 - n !
	." number string = " n  @ .  max_datas @ . CR 
\ Убрано создаем табличку 
	n @ 10 +  row @ LoadDatas:
	n @ 1 - 	num_datas !	

\ табличка есть, добавляем в значения  с ограничением по её размеру !
	max_datas @ n @ MIN 	num_datas !	
	 file @ CLOSE-FILE THROW
	sadr @ STR@ R/O  OPEN-FILE-SHARED THROW \ OPEN-FILE \ CREATE-FILE 
	file !
	
	num_datas  @ 0 	>
	IF
	   num_datas  @ 0 	
    	   DO 
\ читаем построчно и пытаемся запихнуть данные в таблицу
		buf ADDR 255 file @ READ-LINE THROW DROP  n ! 	
		 buf ADDR n @ TYPE 
		 row @ 0 
		 DO I .
			buf ADDR n @ STR>S
			I N_S_PickUpWord S>FLOAT  CR
			IF 
				FDUP F.
				 J I  adr_data_in_number F!	 
			THEN 
		LOOP
		CR 
	   LOOP
	THEN
	 file @ CLOSE-FILE THROW
sadr @ STRFREE	
 SeeDatas 
 
;

;CLASS

: LoadDatas: ROT ^ LoadDatas: ; ( obj line col -- )
: LoadData tekdata  ^ LoadData ;  ( data1 .. dataN -- )
: LoadData: tekdata  ^ LoadData: ;   ( data1 .. dataN -- )
: SeeDatas   ^ SeeDatas ;
: LoadFile  tekdata  ^ LoadFile  ;
 
\EOF

 tabl_kalibr NEW kalibrovka 

 tabl_kalibr NEW k12 
\ kalibrovka  .
\  kalibrovka  see tabl_kalibr NEW kalibrovka 

\ 0 , HERE  1024  ALLOT VALUE kalibrovka

CR
10 2 kalibrovka   LoadDatas:
 80.000e6	-20.6E	LoadData
 300.000e6	-9.9E	LoadData
600.259e6	-14.4E	LoadData
1.000e9        -10.1E	LoadData
1.000e9        -1.1E	LoadData
2.000e9        -2.1E	LoadData
3.000e9        -3.1E	LoadData
4.000e9        -4.1E	LoadData

\ SaveFreq 

\ EOF
kalibrovka SeeDatas
CR
  100.000e6 1 kalibrovka TakeData  F.
  CR
  1000.000e6 1 kalibrovka TakeData  F.
  CR
  1000.000e6 1 kalibrovka TakeData  F.
  CR
  1e9 kalibrovka SaveFreq
  1000.000e6 1 kalibrovka TakeData  F.
  CR
  
\ EOF
\  0 , HERE  1024  ALLOT VALUE k12
10 2 k12  LoadDatas:
 1.000e6	 1.E	LoadData
 2.000e6	 2.E	LoadData
 3.000e6	 3.e	LoadData
 4.000e6	 4.E	LoadData
 5.000e6	 5.E	LoadData
 6.000e6	 6.E	LoadData
 7.000e6	 7.E	LoadData
k12 SeeDatas

: z 
4.5000e6	1 k12 TakeData  F.
 0.000e6		1 k12 TakeData F. 
 8.000e6		1 k12 TakeData  F. 
; 
CR

tabl_kalibr NEW k0 

30 3 k0  LoadDatas:
 LoadData:   1.000e6  1.2E	10.5E
 LoadData:   2.000e6  3.E	10.6E
 LoadData:   3.000e6  4.E	10.7E
 k0 SeeDatas
  1.000e6 1 k0 TakeData  F.
CR
\ EOF  



tabl_kalibr NEW k1 

 \ " data.txt" outFileOpen VALUE file
\ ' print 
 4 " t.txt" k1 LoadFile
