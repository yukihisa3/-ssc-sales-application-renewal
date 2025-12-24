# SKE0060B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKE0060B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷検品サブシステム              *
*    業務名　　　　　　　：　出荷検品                          *
*    モジュール名　　　　：　送信検品グループ作成              *
*    作成日／更新日　　　：　2000/10/04                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　倉庫別検品取引先設定Ｍを順読みし、*
*                            倉庫毎に登録されている検品グループ*
*                            マスタを読み、条件Ｆの出力番号によ*
*                            りレコードを出力する。            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE0060B.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          00/10/04.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA       IS        YA
     YB-21    IS        YB-21
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*検品グループマスタ
     SELECT   SOKKPGF   ASSIGN    TO        DA-01-VI-SOKKPGL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KPG-F01   KPG-F02
                        FILE STATUS    IS   KPG-STATUS.
*条件ファイル
     SELECT   JYOKENF   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01
                                            JYO-F02
                        FILE  STATUS   IS   JYO-STATUS.
*送信用データ１
     SELECT   KNPKPG01  ASSIGN    TO        KNPKPG01
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D01-STATUS.
*送信用データ２
     SELECT   KNPKPG02  ASSIGN    TO        KNPKPG02
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D02-STATUS.
*送信用データ３
     SELECT   KNPKPG03  ASSIGN    TO        KNPKPG03
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D03-STATUS.
*送信用データ４
     SELECT   KNPKPG04  ASSIGN    TO        KNPKPG04
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D04-STATUS.
*送信用データ５
     SELECT   KNPKPG05  ASSIGN    TO        KNPKPG05
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D05-STATUS.
*送信用伝票データ６
     SELECT   KNPKPG06  ASSIGN    TO        KNPKPG06
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D06-STATUS.
*送信用伝票データ７
     SELECT   KNPKPG07  ASSIGN    TO        KNPKPG07
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D07-STATUS.
*送信用伝票データ８
     SELECT   KNPKPG08  ASSIGN    TO        KNPKPG08
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D08-STATUS.
*送信用伝票データ９
     SELECT   KNPKPG09  ASSIGN    TO        KNPKPG09
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D09-STATUS.
*送信用伝票データ１０
     SELECT   KNPKPG10  ASSIGN    TO        KNPKPG10
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D10-STATUS.
*送信用伝票データ１１
     SELECT   KNPKPG11  ASSIGN    TO        KNPKPG11
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D11-STATUS.
*送信用伝票データ１２
     SELECT   KNPKPG12  ASSIGN    TO        KNPKPG12
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D12-STATUS.
*送信用伝票データ１３
     SELECT   KNPKPG13  ASSIGN    TO        KNPKPG13
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D13-STATUS.
*送信用伝票データ１４
     SELECT   KNPKPG14  ASSIGN    TO        KNPKPG14
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D14-STATUS.
*送信用伝票データ１５
     SELECT   KNPKPG15  ASSIGN    TO        KNPKPG15
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D15-STATUS.
*送信用伝票データ１６
     SELECT   KNPKPG16  ASSIGN    TO        KNPKPG16
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D16-STATUS.
*送信用伝票データ１７
     SELECT   KNPKPG17  ASSIGN    TO        KNPKPG17
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D17-STATUS.
*送信用伝票データ１８
     SELECT   KNPKPG18  ASSIGN    TO        KNPKPG18
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D18-STATUS.
*送信用伝票データ１９
     SELECT   KNPKPG19  ASSIGN    TO        KNPKPG19
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D19-STATUS.
*送信用伝票データ２０
     SELECT   KNPKPG20  ASSIGN    TO        KNPKPG20
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D20-STATUS.
*件数ファイル０１
     SELECT   KNPKPK01  ASSIGN    TO        KNPKPK01
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K01-STATUS.
*件数ファイル０２
     SELECT   KNPKPK02  ASSIGN    TO        KNPKPK02
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K02-STATUS.
*件数ファイル０３
     SELECT   KNPKPK03  ASSIGN    TO        KNPKPK03
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K03-STATUS.
*件数ファイル０４
     SELECT   KNPKPK04  ASSIGN    TO        KNPKPK04
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K04-STATUS.
*件数ファイル０５
     SELECT   KNPKPK05  ASSIGN    TO        KNPKPK05
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K05-STATUS.
*件数ファイル０６
     SELECT   KNPKPK06  ASSIGN    TO        KNPKPK06
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K06-STATUS.
*件数ファイル０７
     SELECT   KNPKPK07  ASSIGN    TO        KNPKPK07
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K07-STATUS.
*件数ファイル０８
     SELECT   KNPKPK08  ASSIGN    TO        KNPKPK08
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K08-STATUS.
*件数ファイル０９
     SELECT   KNPKPK09  ASSIGN    TO        KNPKPK09
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K09-STATUS.
*件数ファイル１０
     SELECT   KNPKPK10  ASSIGN    TO        KNPKPK10
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K10-STATUS.
*件数ファイル１１
     SELECT   KNPKPK11  ASSIGN    TO        KNPKPK11
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K11-STATUS.
*件数ファイル１２
     SELECT   KNPKPK12  ASSIGN    TO        KNPKPK12
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K12-STATUS.
*件数ファイル１３
     SELECT   KNPKPK13  ASSIGN    TO        KNPKPK13
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K13-STATUS.
*件数ファイル１４
     SELECT   KNPKPK14  ASSIGN    TO        KNPKPK14
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K14-STATUS.
*件数ファイル１５
     SELECT   KNPKPK15  ASSIGN    TO        KNPKPK15
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K15-STATUS.
*件数ファイル１６
     SELECT   KNPKPK16  ASSIGN    TO        KNPKPK16
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K16-STATUS.
*件数ファイル１７
     SELECT   KNPKPK17  ASSIGN    TO        KNPKPK17
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K17-STATUS.
*件数ファイル１８
     SELECT   KNPKPK18  ASSIGN    TO        KNPKPK18
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K18-STATUS.
*件数ファイル１９
     SELECT   KNPKPK19  ASSIGN    TO        KNPKPK19
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K19-STATUS.
*件数ファイル２０
     SELECT   KNPKPK20  ASSIGN    TO        KNPKPK20
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K20-STATUS.
*プリント Ｆ
     SELECT      PRINTF      ASSIGN    TO        LP-04-PRTF.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
*
******************************************************************
*    検品グループマスタ
******************************************************************
 FD  SOKKPGF             LABEL RECORD   IS   STANDARD.
     COPY     SOKKPGF   OF        XFDLIB
              JOINING   KPG       PREFIX.
*
******************************************************************
*    条件ファイル
******************************************************************
 FD  JYOKENF
                        LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO  AS   PREFIX.
*
******************************************************************
*    送信用店舗データ１
******************************************************************
 FD  KNPKPG01           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D01       PREFIX.
******************************************************************
*    送信用店舗データ２
******************************************************************
 FD  KNPKPG02           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D02       PREFIX.
******************************************************************
*    送信用店舗データ３
******************************************************************
 FD  KNPKPG03           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D03       PREFIX.
******************************************************************
*    送信用店舗データ４
******************************************************************
 FD  KNPKPG04           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D04       PREFIX.
******************************************************************
*    送信用店舗データ５
******************************************************************
 FD  KNPKPG05           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D05       PREFIX.
******************************************************************
*    送信用店舗データ６
******************************************************************
 FD  KNPKPG06           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D06       PREFIX.
******************************************************************
*    送信用店舗データ７
******************************************************************
 FD  KNPKPG07           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D07       PREFIX.
******************************************************************
*    送信用店舗データ８
******************************************************************
 FD  KNPKPG08           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D08       PREFIX.
******************************************************************
*    送信用店舗データ９
******************************************************************
 FD  KNPKPG09           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D09       PREFIX.
******************************************************************
*    送信用店舗データ１０
******************************************************************
 FD  KNPKPG10           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D10       PREFIX.
******************************************************************
*    送信用店舗データ１１
******************************************************************
 FD  KNPKPG11           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D11       PREFIX.
******************************************************************
*    送信用店舗データ１２
******************************************************************
 FD  KNPKPG12           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D12       PREFIX.
******************************************************************
*    送信用店舗データ１３
******************************************************************
 FD  KNPKPG13           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D13       PREFIX.
******************************************************************
*    送信用店舗データ１４
******************************************************************
 FD  KNPKPG14           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D14       PREFIX.
******************************************************************
*    送信用店舗データ１５
******************************************************************
 FD  KNPKPG15           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D15       PREFIX.
******************************************************************
*    送信用店舗データ１６
******************************************************************
 FD  KNPKPG16           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D16       PREFIX.
******************************************************************
*    送信用店舗データ１７
******************************************************************
 FD  KNPKPG17           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D17       PREFIX.
******************************************************************
*    送信用店舗データ１８
******************************************************************
 FD  KNPKPG18           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D18       PREFIX.
******************************************************************
*    送信用店舗データ１９
******************************************************************
 FD  KNPKPG19           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D19       PREFIX.
******************************************************************
*    送信用店舗データ２０
******************************************************************
 FD  KNPKPG20           LABEL RECORD   IS   STANDARD.
     COPY     KNPKPGF   OF        XFDLIB
              JOINING   D20       PREFIX.
******************************************************************
*    件数データ０１
******************************************************************
 FD  KNPKPK01           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K01-REC.
     03  K01-F01             PIC  9(08).
******************************************************************
*    件数データ０２
******************************************************************
 FD  KNPKPK02           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K02-REC.
     03  K02-F01             PIC  9(08).
******************************************************************
*    件数データ０３
******************************************************************
 FD  KNPKPK03           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K03-REC.
     03  K03-F01             PIC  9(08).
******************************************************************
*    件数データ０４
******************************************************************
 FD  KNPKPK04           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K04-REC.
     03  K04-F01             PIC  9(08).
******************************************************************
*    件数データ０５
******************************************************************
 FD  KNPKPK05           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K05-REC.
     03  K05-F01             PIC  9(08).
******************************************************************
*    件数データ０６
******************************************************************
 FD  KNPKPK06           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K06-REC.
     03  K06-F01             PIC  9(08).
******************************************************************
*    件数データ０７
******************************************************************
 FD  KNPKPK07           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K07-REC.
     03  K07-F01             PIC  9(08).
******************************************************************
*    件数データ０８
******************************************************************
 FD  KNPKPK08           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K08-REC.
     03  K08-F01             PIC  9(08).
******************************************************************
*    件数データ０９
******************************************************************
 FD  KNPKPK09           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K09-REC.
     03  K09-F01             PIC  9(08).
******************************************************************
*    件数データ１０
******************************************************************
 FD  KNPKPK10           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K10-REC.
     03  K10-F01             PIC  9(08).
******************************************************************
*    件数データ１１
******************************************************************
 FD  KNPKPK11           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K11-REC.
     03  K11-F01             PIC  9(08).
******************************************************************
*    件数データ１２
******************************************************************
 FD  KNPKPK12           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K12-REC.
     03  K12-F01             PIC  9(08).
******************************************************************
*    件数データ１３
******************************************************************
 FD  KNPKPK13           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K13-REC.
     03  K13-F01             PIC  9(08).
******************************************************************
*    件数データ１４
******************************************************************
 FD  KNPKPK14           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K14-REC.
     03  K14-F01             PIC  9(08).
******************************************************************
*    件数データ１５
******************************************************************
 FD  KNPKPK15           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K15-REC.
     03  K15-F01             PIC  9(08).
******************************************************************
*    件数データ１６
******************************************************************
 FD  KNPKPK16           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K16-REC.
     03  K16-F01             PIC  9(08).
******************************************************************
*    件数データ１７
******************************************************************
 FD  KNPKPK17           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K17-REC.
     03  K17-F01             PIC  9(08).
******************************************************************
*    件数データ１８
******************************************************************
 FD  KNPKPK18           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K18-REC.
     03  K18-F01             PIC  9(08).
******************************************************************
*    件数データ１９
******************************************************************
 FD  KNPKPK19           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K19-REC.
     03  K19-F01             PIC  9(08).
******************************************************************
*    件数データ２０
******************************************************************
 FD  KNPKPK20           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K20-REC.
     03  K20-F01             PIC  9(08).
****************************************************************
*    FILE = プリント　ファイル                                 *
****************************************************************
 FD  PRINTF.
 01  PRINT-REC                    PIC       X(200).
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  KENPF-FLG               PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  IX                      PIC  9(02)     VALUE  ZERO.
 01  WK-SOKCD                PIC  X(02)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE.
         05  SYS-YY        PIC 9(02).
         05  SYS-MM        PIC 9(02).
         05  SYS-DD        PIC 9(02).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  KPG-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  D01-STATUS        PIC  X(02).
     03  D02-STATUS        PIC  X(02).
     03  D03-STATUS        PIC  X(02).
     03  D04-STATUS        PIC  X(02).
     03  D05-STATUS        PIC  X(02).
     03  D06-STATUS        PIC  X(02).
     03  D07-STATUS        PIC  X(02).
     03  D08-STATUS        PIC  X(02).
     03  D09-STATUS        PIC  X(02).
     03  D10-STATUS        PIC  X(02).
     03  D11-STATUS        PIC  X(02).
     03  D12-STATUS        PIC  X(02).
     03  D13-STATUS        PIC  X(02).
     03  D14-STATUS        PIC  X(02).
     03  D15-STATUS        PIC  X(02).
     03  D16-STATUS        PIC  X(02).
     03  D17-STATUS        PIC  X(02).
     03  D18-STATUS        PIC  X(02).
     03  D19-STATUS        PIC  X(02).
     03  D20-STATUS        PIC  X(02).
     03  K01-STATUS        PIC  X(02).
     03  K02-STATUS        PIC  X(02).
     03  K03-STATUS        PIC  X(02).
     03  K04-STATUS        PIC  X(02).
     03  K05-STATUS        PIC  X(02).
     03  K06-STATUS        PIC  X(02).
     03  K07-STATUS        PIC  X(02).
     03  K08-STATUS        PIC  X(02).
     03  K09-STATUS        PIC  X(02).
     03  K10-STATUS        PIC  X(02).
     03  K11-STATUS        PIC  X(02).
     03  K12-STATUS        PIC  X(02).
     03  K13-STATUS        PIC  X(02).
     03  K14-STATUS        PIC  X(02).
     03  K15-STATUS        PIC  X(02).
     03  K16-STATUS        PIC  X(02).
     03  K17-STATUS        PIC  X(02).
     03  K18-STATUS        PIC  X(02).
     03  K19-STATUS        PIC  X(02).
     03  K20-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SKE0060B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKE0060B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKE0060B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  WK-FLCD.
         05  WK-FLCD1        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD2        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD3        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD4        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD5        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD6        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD7        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD8        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD9        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD10       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD11       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD12       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD13       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD14       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD15       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD16       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD17       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD18       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD19       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD20       PIC  9(05)  VALUE  ZERO.
     03  WK-FLCDR  REDEFINES WK-FLCD.
         05  WK-FLCDT         PIC  9(05)  OCCURS 20.
     03  WK-RTCD.
         05  WK-RTCD1        PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD2        PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD3        PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD4        PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD5        PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD6        PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD7        PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD8        PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD9        PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD10       PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD11       PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD12       PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD13       PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD14       PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD15       PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD16       PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD17       PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD18       PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD19       PIC  X(02)  VALUE  ZERO.
         05  WK-RTCD20       PIC  X(02)  VALUE  ZERO.
     03  WK-RTCDR  REDEFINES WK-RTCD.
         05  WK-RTCDT        PIC  X(02)  OCCURS 20.
     03  WK-RTNM.
         05  WK-RTNM1        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM2        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM3        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM4        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM5        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM6        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM7        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM8        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM9        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM10       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM11       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM12       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM13       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM14       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM15       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM16       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM17       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM18       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM19       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM20       PIC  N(10)  VALUE  SPACE.
     03  WK-RTNMR  REDEFINES WK-RTNM.
         05  WK-RTNMT        PIC  N(10)  OCCURS 20.
     03  WK-RTCNT.
         05  WK-RTCNT1       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT2       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT3       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT4       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT5       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT6       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT7       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT8       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT9       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT10      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT11      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT12      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT13      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT14      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT15      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT16      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT17      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT18      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT19      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT20      PIC  9(05)  VALUE  ZERO.
     03  WK-RTCNTR REDEFINES WK-RTCNT.
         05  WK-RTCNTT       PIC  9(05)  OCCURS 20.
*    見出し行１
 01  MIDASHI1.
     03  FILLER                   PIC       X(37)  VALUE SPACE.
     03  FILLER                   PIC       N(18)  VALUE
       NC"【　出荷検品検品Ｇ振分件数リスト　】"
       CHARACTER   TYPE   IS   YB-21.
     03  FILLER                   PIC       X(17)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "DATE:".
     03  YY                       PIC       99.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  MM                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  DD                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "PAGE:".
     03  PEIJI                    PIC       ZZZ9.
*    見出し行２
 01  MIDASHI2           CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(40)  VALUE SPACE.
     03  FILLER                   PIC       X(08)  VALUE
         "FILE-NO.".
     03  FILLER                   PIC       X(02)  VALUE SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"場所ＣＤ".
     03  FILLER                   PIC       X(02)  VALUE SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"出荷場所名".
     03  FILLER                   PIC       X(12)  VALUE SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"データ件数".
     03  FILLER                   PIC       X(78)  VALUE SPACE.
*    明細行
 01  MEISAI             CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(41)  VALUE SPACE.
     03  FILECD                   PIC       ZZ,ZZ9.
     03  FILLER                   PIC       X(08)  VALUE SPACE.
*****03  ROUTECD                  PIC       9(02).
     03  ROUTECD                  PIC       X(02).
     03  FILLER                   PIC       X(04)  VALUE SPACE.
     03  ROUTENM                  PIC       N(10).
     03  FILLER                   PIC       X(04)  VALUE SPACE.
     03  DATASU                   PIC       ZZ,ZZ9.
     03  FILLER                   PIC       X(78)  VALUE SPACE.
*    線１
 01  SEN1               CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                   PIC       N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                   PIC       N(18)  VALUE
         NC"──────────────────".
*    線２
 01  SEN2.
     03  FILLER                   PIC       X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC       X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC       X(36)  VALUE
         "------------------------------------".
*
     COPY   KNPKPGF  OF XFDLIB  JOINING   WK  AS   PREFIX.
*    名称編集１
 01  WK-HEN1.
     03  WK-HEN1-1          PIC   X(01).
     03  WK-HEN1-2          PIC   N(15).
     03  WK-HEN1-3          PIC   X(01).
*    名称編集２
 01  WK-HEN2.
     03  WK-HEN2-1          PIC   X(01).
     03  WK-HEN2-2          PIC   N(10).
     03  WK-HEN2-3          PIC   X(01).
*    名称編集２
 01  WK-HEN-DATE.
     03  WK-HEN-DATE1       PIC   X(04).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE2       PIC   X(02).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE3       PIC   X(02).
*    日付変換１
 01  WK-HENKAN              PIC   9(08)  VALUE  ZERO.
 01  WK-TOUROKU.
     03  WK-TOUROKU1        PIC   9(04).
     03  WK-TOUROKU2        PIC   9(02).
     03  WK-TOUROKU3        PIC   9(02).
*    日付変換２
 01  WK-KOUSIN.
     03  WK-KOUSIN1         PIC   9(04).
     03  WK-KOUSIN2         PIC   9(02).
     03  WK-KOUSIN3         PIC   9(02).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG01.
     MOVE      "KNPKPG01"   TO   AB-FILE.
     MOVE      D01-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG02.
     MOVE      "KNPKPG02"   TO   AB-FILE.
     MOVE      D02-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG03.
     MOVE      "KNPKPG03"   TO   AB-FILE.
     MOVE      D03-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG04.
     MOVE      "KNPKPG04"   TO   AB-FILE.
     MOVE      D04-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG05.
     MOVE      "KNPKPG05"   TO   AB-FILE.
     MOVE      D05-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG06.
     MOVE      "KNPKPG06"   TO   AB-FILE.
     MOVE      D06-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG07.
     MOVE      "KNPKPG07"   TO   AB-FILE.
     MOVE      D07-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG08.
     MOVE      "KNPKPG08"   TO   AB-FILE.
     MOVE      D08-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG09.
     MOVE      "KNPKPG09"   TO   AB-FILE.
     MOVE      D09-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG10.
     MOVE      "KNPKPG10"   TO   AB-FILE.
     MOVE      D10-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG11.
     MOVE      "KNPKPG11"   TO   AB-FILE.
     MOVE      D11-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC12          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG12.
     MOVE      "KNPKPG12"   TO   AB-FILE.
     MOVE      D12-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC13          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG13.
     MOVE      "KNPKPG13"   TO   AB-FILE.
     MOVE      D13-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC14          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG14.
     MOVE      "KNPKPG14"   TO   AB-FILE.
     MOVE      D14-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC15          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG15.
     MOVE      "KNPKPG15"   TO   AB-FILE.
     MOVE      D15-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC16          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG16.
     MOVE      "KNPKPG16"   TO   AB-FILE.
     MOVE      D16-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC17          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG17.
     MOVE      "KNPKPG17"   TO   AB-FILE.
     MOVE      D17-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC18          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG18.
     MOVE      "KNPKPG18"   TO   AB-FILE.
     MOVE      D18-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC19          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG19.
     MOVE      "KNPKPG19"   TO   AB-FILE.
     MOVE      D19-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC20          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPG20.
     MOVE      "KNPKPG20"   TO   AB-FILE.
     MOVE      D20-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC21          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JYOKENF.
     MOVE      "JYOKENF "   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC22          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SOKKPGF.
     MOVE      "SOKKPGF  "   TO   AB-FILE.
     MOVE      KPG-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC23          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK01.
     MOVE      "KNPKPK01"   TO   AB-FILE.
     MOVE      K01-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC24          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK02.
     MOVE      "KNPKPK02"   TO   AB-FILE.
     MOVE      K02-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC25          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK03.
     MOVE      "KNPKPK03"   TO   AB-FILE.
     MOVE      K03-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC26          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK04.
     MOVE      "KNPKPK04"   TO   AB-FILE.
     MOVE      K04-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC27          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK05.
     MOVE      "KNPKPK05"   TO   AB-FILE.
     MOVE      K05-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC28          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK06.
     MOVE      "KNPKPK06"   TO   AB-FILE.
     MOVE      K06-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC29          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK07.
     MOVE      "KNPKPK07"   TO   AB-FILE.
     MOVE      K07-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC30          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK08.
     MOVE      "KNPKPK08"   TO   AB-FILE.
     MOVE      K08-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC31          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK09.
     MOVE      "KNPKPK09"   TO   AB-FILE.
     MOVE      K09-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC32          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK10.
     MOVE      "KNPKPK10"   TO   AB-FILE.
     MOVE      K10-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC33          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK11.
     MOVE      "KNPKPK11"   TO   AB-FILE.
     MOVE      K11-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC34          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK12.
     MOVE      "KNPKPK12"   TO   AB-FILE.
     MOVE      K12-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC35          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK13.
     MOVE      "KNPKPK13"   TO   AB-FILE.
     MOVE      K13-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC36          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK14.
     MOVE      "KNPKPK14"   TO   AB-FILE.
     MOVE      K14-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC37          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK15.
     MOVE      "KNPKPK15"   TO   AB-FILE.
     MOVE      K15-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC38          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK16.
     MOVE      "KNPKPK16"   TO   AB-FILE.
     MOVE      K16-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC39          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK17.
     MOVE      "KNPKPK17"   TO   AB-FILE.
     MOVE      K17-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC40          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK18.
     MOVE      "KNPKPK18"   TO   AB-FILE.
     MOVE      K18-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC41          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK19.
     MOVE      "KNPKPK19"   TO   AB-FILE.
     MOVE      K19-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC42          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNPKPK20.
     MOVE      "KNPKPK20"   TO   AB-FILE.
     MOVE      K20-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     JYOKENF SOKKPGF.
     OPEN     OUTPUT    KNPKPG01.
     OPEN     OUTPUT    KNPKPG02.
     OPEN     OUTPUT    KNPKPG03.
     OPEN     OUTPUT    KNPKPG04.
     OPEN     OUTPUT    KNPKPG05.
     OPEN     OUTPUT    KNPKPG06.
     OPEN     OUTPUT    KNPKPG07.
     OPEN     OUTPUT    KNPKPG08.
     OPEN     OUTPUT    KNPKPG09.
     OPEN     OUTPUT    KNPKPG10.
     OPEN     OUTPUT    KNPKPG11.
     OPEN     OUTPUT    KNPKPG12.
     OPEN     OUTPUT    KNPKPG13.
     OPEN     OUTPUT    KNPKPG14.
     OPEN     OUTPUT    KNPKPG15.
     OPEN     OUTPUT    KNPKPG16.
     OPEN     OUTPUT    KNPKPG17.
     OPEN     OUTPUT    KNPKPG18.
     OPEN     OUTPUT    KNPKPG19.
     OPEN     OUTPUT    KNPKPG20.
     OPEN     OUTPUT    KNPKPK01.
     OPEN     OUTPUT    KNPKPK02.
     OPEN     OUTPUT    KNPKPK03.
     OPEN     OUTPUT    KNPKPK04.
     OPEN     OUTPUT    KNPKPK05.
     OPEN     OUTPUT    KNPKPK06.
     OPEN     OUTPUT    KNPKPK07.
     OPEN     OUTPUT    KNPKPK08.
     OPEN     OUTPUT    KNPKPK09.
     OPEN     OUTPUT    KNPKPK10.
     OPEN     OUTPUT    KNPKPK11.
     OPEN     OUTPUT    KNPKPK12.
     OPEN     OUTPUT    KNPKPK13.
     OPEN     OUTPUT    KNPKPK14.
     OPEN     OUTPUT    KNPKPK15.
     OPEN     OUTPUT    KNPKPK16.
     OPEN     OUTPUT    KNPKPK17.
     OPEN     OUTPUT    KNPKPK18.
     OPEN     OUTPUT    KNPKPK19.
     OPEN     OUTPUT    KNPKPK20.
*****OPEN     OUTPUT    PRINTF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG    RD-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*    検品グループマスタ初期読み
     PERFORM     SOKKPGF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    条件Ｆのブレイクチェック
     IF     ( KPG-F01  NOT =  WK-SOKCD )
*             倉庫ＣＤ入れ替え
              MOVE     KPG-F01    TO   WK-SOKCD
*             出力ファイル番号取得
              PERFORM  JYOKENF-READ-SEC
     END-IF.
*    検品Ｇ情報セット
     PERFORM KENPIN-SET-SEC
*    ファイル出力テーブル判定
     PERFORM TBLSET-SEC
*    検品グループマスタ出力（１件目）
     PERFORM  SOKKPGF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*出力件数リスト出力
     PERFORM   LISTWT-SEC.
*
     CLOSE     JYOKENF  SOKKPGF.
     CLOSE     KNPKPG01.
     CLOSE     KNPKPG02.
     CLOSE     KNPKPG03.
     CLOSE     KNPKPG04.
     CLOSE     KNPKPG05.
     CLOSE     KNPKPG06.
     CLOSE     KNPKPG07.
     CLOSE     KNPKPG08.
     CLOSE     KNPKPG09.
     CLOSE     KNPKPG10.
     CLOSE     KNPKPG11.
     CLOSE     KNPKPG12.
     CLOSE     KNPKPG13.
     CLOSE     KNPKPG14.
     CLOSE     KNPKPG15.
     CLOSE     KNPKPG16.
     CLOSE     KNPKPG17.
     CLOSE     KNPKPG18.
     CLOSE     KNPKPG19.
     CLOSE     KNPKPG20.
     CLOSE     KNPKPK01.
     CLOSE     KNPKPK02.
     CLOSE     KNPKPK03.
     CLOSE     KNPKPK04.
     CLOSE     KNPKPK05.
     CLOSE     KNPKPK06.
     CLOSE     KNPKPK07.
     CLOSE     KNPKPK08.
     CLOSE     KNPKPK09.
     CLOSE     KNPKPK10.
     CLOSE     KNPKPK11.
     CLOSE     KNPKPK12.
     CLOSE     KNPKPK13.
     CLOSE     KNPKPK14.
     CLOSE     KNPKPK15.
     CLOSE     KNPKPK16.
     CLOSE     KNPKPK17.
     CLOSE     KNPKPK18.
     CLOSE     KNPKPK19.
     CLOSE     KNPKPK20.
*****CLOSE     PRINTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　倉庫出力先ファイル判定→ファイル出力            *
****************************************************************
 KENPIN-SET-SEC         SECTION.
*
     MOVE     "KENPIN-SET-SEC"  TO      S-NAME.
*    ワークエリア初期化
     MOVE      SPACE            TO      WK-REC.
     INITIALIZE                         WK-REC.
*    項目セット
*    倉庫ＣＤ
     MOVE      KPG-F01          TO      WK-F01.
*    検品グループＧＣＤ
     MOVE      KPG-F02          TO      WK-F02.
*    検品グループＧ名
     MOVE      KPG-F03          TO      WK-HEN2-2.
     MOVE      X"28"            TO      WK-HEN2-1.
     MOVE      X"29"            TO      WK-HEN2-3.
     MOVE      WK-HEN2          TO      WK-F03.
*    ピッキング種別
     MOVE      KPG-F04          TO      WK-F04.
*    登録日付
     MOVE      KPG-F05          TO      WK-HENKAN.
     MOVE      WK-HENKAN        TO      WK-TOUROKU.
     MOVE      WK-TOUROKU1      TO      WK-HEN-DATE1.
     MOVE      WK-TOUROKU2      TO      WK-HEN-DATE2.
     MOVE      WK-TOUROKU3      TO      WK-HEN-DATE3.
     MOVE      WK-HEN-DATE      TO      WK-F05.
*    更新日付
     MOVE      KPG-F07          TO      WK-HENKAN.
     MOVE      WK-HENKAN        TO      WK-KOUSIN.
     MOVE      WK-KOUSIN1       TO      WK-HEN-DATE1.
     MOVE      WK-KOUSIN2       TO      WK-HEN-DATE2.
     MOVE      WK-KOUSIN3       TO      WK-HEN-DATE3.
     MOVE      WK-HEN-DATE      TO      WK-F06.
*    改行コード
     MOVE      X"0D0A"          TO      WK-F07.
*
 KENPIN-SET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　倉庫出力先ファイル判定→ファイル出力            *
****************************************************************
 TBLSET-SEC            SECTION.
*
     MOVE     "TBLSET-SEC"   TO      S-NAME.
*送信用伝票データ出力
     EVALUATE JYO-F04
         WHEN 1
              MOVE     SPACE          TO   D01-REC
              INITIALIZE                   D01-REC
              MOVE     WK-REC         TO   D01-REC
              WRITE    D01-REC
              ADD      1              TO   WK-RTCNT1
         WHEN 2
              MOVE     SPACE          TO   D02-REC
              INITIALIZE                   D02-REC
              MOVE     WK-REC         TO   D02-REC
              WRITE    D02-REC
              ADD      1              TO   WK-RTCNT2
         WHEN 3
              MOVE     SPACE          TO   D03-REC
              INITIALIZE                   D03-REC
              MOVE     WK-REC         TO   D03-REC
              WRITE    D03-REC
              ADD      1              TO   WK-RTCNT3
         WHEN 4
              MOVE     SPACE          TO   D04-REC
              INITIALIZE                   D04-REC
              MOVE     WK-REC         TO   D04-REC
              WRITE    D04-REC
              ADD      1              TO   WK-RTCNT4
         WHEN 5
              MOVE     SPACE          TO   D05-REC
              INITIALIZE                   D05-REC
              MOVE     WK-REC         TO   D05-REC
              WRITE    D05-REC
              ADD      1              TO   WK-RTCNT5
         WHEN 6
              MOVE     SPACE          TO   D06-REC
              INITIALIZE                   D06-REC
              MOVE     WK-REC         TO   D06-REC
              WRITE    D06-REC
              ADD      1              TO   WK-RTCNT6
         WHEN 7
              MOVE     SPACE          TO   D07-REC
              INITIALIZE                   D07-REC
              MOVE     WK-REC         TO   D07-REC
              WRITE    D07-REC
              ADD      1              TO   WK-RTCNT7
         WHEN 8
              MOVE     SPACE          TO   D08-REC
              INITIALIZE                   D08-REC
              MOVE     WK-REC         TO   D08-REC
              WRITE    D08-REC
              ADD      1              TO   WK-RTCNT9
         WHEN 9
              MOVE     SPACE          TO   D09-REC
              INITIALIZE                   D09-REC
              MOVE     WK-REC         TO   D09-REC
              WRITE    D09-REC
              ADD      1              TO   WK-RTCNT9
         WHEN 10
              MOVE     SPACE          TO   D10-REC
              INITIALIZE                   D10-REC
              MOVE     WK-REC         TO   D10-REC
              WRITE    D10-REC
              ADD      1              TO   WK-RTCNT10
         WHEN 11
              MOVE     SPACE          TO   D11-REC
              INITIALIZE                   D11-REC
              MOVE     WK-REC         TO   D11-REC
              WRITE    D11-REC
              ADD      1              TO   WK-RTCNT11
         WHEN 12
              MOVE     SPACE          TO   D12-REC
              INITIALIZE                   D12-REC
              MOVE     WK-REC         TO   D12-REC
              WRITE    D12-REC
              ADD      1              TO   WK-RTCNT12
         WHEN 13
              MOVE     SPACE          TO   D13-REC
              INITIALIZE                   D13-REC
              MOVE     WK-REC         TO   D13-REC
              WRITE    D13-REC
              ADD      1              TO   WK-RTCNT13
         WHEN 14
              MOVE     SPACE          TO   D14-REC
              INITIALIZE                   D14-REC
              MOVE     WK-REC         TO   D14-REC
              WRITE    D14-REC
              ADD      1              TO   WK-RTCNT14
         WHEN 15
              MOVE     SPACE          TO   D15-REC
              INITIALIZE                   D15-REC
              MOVE     WK-REC         TO   D15-REC
              WRITE    D15-REC
              ADD      1              TO   WK-RTCNT15
         WHEN 16
              MOVE     SPACE          TO   D16-REC
              INITIALIZE                   D16-REC
              MOVE     WK-REC         TO   D16-REC
              WRITE    D16-REC
              ADD      1              TO   WK-RTCNT16
         WHEN 17
              MOVE     SPACE          TO   D17-REC
              INITIALIZE                   D17-REC
              MOVE     WK-REC         TO   D17-REC
              WRITE    D17-REC
              ADD      1              TO   WK-RTCNT17
         WHEN 18
              MOVE     SPACE          TO   D18-REC
              INITIALIZE                   D18-REC
              MOVE     WK-REC         TO   D18-REC
              WRITE    D18-REC
              ADD      1              TO   WK-RTCNT18
         WHEN 19
              MOVE     SPACE          TO   D19-REC
              INITIALIZE                   D19-REC
              MOVE     WK-REC         TO   D19-REC
              WRITE    D19-REC
              ADD      1              TO   WK-RTCNT19
         WHEN 20
              MOVE     SPACE          TO   D20-REC
              INITIALIZE                   D20-REC
              MOVE     WK-REC         TO   D20-REC
              WRITE    D20-REC
              ADD      1              TO   WK-RTCNT20
     END-EVALUATE.
*
 TBLSET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　条件ファイル読込み　　　　　　　　　　　　　　*
****************************************************************
 JYOKENF-READ-SEC      SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      "20"      TO      JYO-F01.
     MOVE      KPG-F01   TO      JYO-F02.
     READ      JYOKENF   INVALID
               DISPLAY "JYOKENF INVALID KEY = "
                        JYO-F01 ":" JYO-F02  UPON CONS
               STOP  RUN
       NOT INVALID
              EVALUATE  JYO-F04
                 WHEN   1   MOVE  JYO-F04  TO  WK-FLCD1
                            MOVE  KPG-F01  TO  WK-RTCD1
                            MOVE  JYO-F03  TO  WK-RTNM1
                 WHEN   2   MOVE  JYO-F04  TO  WK-FLCD2
                            MOVE  KPG-F01  TO  WK-RTCD2
                            MOVE  JYO-F03  TO  WK-RTNM2
                 WHEN   3   MOVE  JYO-F04  TO  WK-FLCD3
                            MOVE  KPG-F01  TO  WK-RTCD3
                            MOVE  JYO-F03  TO  WK-RTNM3
                 WHEN   4   MOVE  JYO-F04  TO  WK-FLCD4
                            MOVE  KPG-F01  TO  WK-RTCD4
                            MOVE  JYO-F03  TO  WK-RTNM4
                 WHEN   5   MOVE  JYO-F04  TO  WK-FLCD5
                            MOVE  KPG-F01  TO  WK-RTCD5
                            MOVE  JYO-F03  TO  WK-RTNM5
                 WHEN   6   MOVE  JYO-F04  TO  WK-FLCD6
                            MOVE  KPG-F01  TO  WK-RTCD6
                            MOVE  JYO-F03  TO  WK-RTNM6
                 WHEN   7   MOVE  JYO-F04  TO  WK-FLCD7
                            MOVE  KPG-F01  TO  WK-RTCD7
                            MOVE  JYO-F03  TO  WK-RTNM7
                 WHEN   8   MOVE  JYO-F04  TO  WK-FLCD8
                            MOVE  KPG-F01  TO  WK-RTCD8
                            MOVE  JYO-F03  TO  WK-RTNM8
                 WHEN   9   MOVE  JYO-F04  TO  WK-FLCD9
                            MOVE  KPG-F01  TO  WK-RTCD9
                            MOVE  JYO-F03  TO  WK-RTNM9
                 WHEN  10   MOVE  JYO-F04  TO  WK-FLCD10
                            MOVE  KPG-F01  TO  WK-RTCD10
                            MOVE  JYO-F03  TO  WK-RTNM10
                 WHEN  11   MOVE  JYO-F04  TO  WK-FLCD11
                            MOVE  KPG-F01  TO  WK-RTCD11
                            MOVE  JYO-F03  TO  WK-RTNM11
                 WHEN  12   MOVE  JYO-F04  TO  WK-FLCD12
                            MOVE  KPG-F01  TO  WK-RTCD12
                            MOVE  JYO-F03  TO  WK-RTNM12
                 WHEN  13   MOVE  JYO-F04  TO  WK-FLCD13
                            MOVE  KPG-F01  TO  WK-RTCD13
                            MOVE  JYO-F03  TO  WK-RTNM13
                 WHEN  14   MOVE  JYO-F04  TO  WK-FLCD14
                            MOVE  KPG-F01  TO  WK-RTCD14
                            MOVE  JYO-F03  TO  WK-RTNM14
                 WHEN  15   MOVE  JYO-F04  TO  WK-FLCD15
                            MOVE  KPG-F01  TO  WK-RTCD15
                            MOVE  JYO-F03  TO  WK-RTNM15
                 WHEN  16   MOVE  JYO-F04  TO  WK-FLCD16
                            MOVE  KPG-F01  TO  WK-RTCD16
                            MOVE  JYO-F03  TO  WK-RTNM16
                 WHEN  17   MOVE  JYO-F04  TO  WK-FLCD17
                            MOVE  KPG-F01  TO  WK-RTCD17
                            MOVE  JYO-F03  TO  WK-RTNM17
                 WHEN  18   MOVE  JYO-F04  TO  WK-FLCD18
                            MOVE  KPG-F01  TO  WK-RTCD18
                            MOVE  JYO-F03  TO  WK-RTNM18
                 WHEN  19   MOVE  JYO-F04  TO  WK-FLCD19
                            MOVE  KPG-F01  TO  WK-RTCD19
                            MOVE  JYO-F03  TO  WK-RTNM19
                 WHEN  20   MOVE  JYO-F04  TO  WK-FLCD20
                            MOVE  KPG-F01  TO  WK-RTCD20
                            MOVE  JYO-F03  TO  WK-RTNM20
              END-EVALUATE
     END-READ.
*
 JYOKENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　検品グループマスタ読込み                        *
****************************************************************
 SOKKPGF-READ-SEC       SECTION.
*
     MOVE     "SOKKPGF-READ-SEC"  TO      S-NAME.
*
     READ      SOKKPGF    AT  END
               MOVE     "END"    TO      END-FLG
     END-READ.
*
 SOKKPGF-READ-EXIT.
     EXIT.
****************************************************************
*           リスト出力処理                          3.1.1      *
****************************************************************
 LISTWT-SEC   SECTION.
*
*****MOVE      SYS-YY         TO        YY.
*****MOVE      SYS-MM         TO        MM.
*****MOVE      SYS-DD         TO        DD.
*****MOVE      1              TO        PEIJI.
*****WRITE     PRINT-REC      FROM      MIDASHI1 AFTER 2.
*****WRITE     PRINT-REC      FROM      SEN1     AFTER 2.
*****WRITE     PRINT-REC      FROM      MIDASHI2 AFTER 1.
*****WRITE     PRINT-REC      FROM      SEN1     AFTER 1.
 LISTWT-010.
     PERFORM   VARYING   IX   FROM      1  BY  1
               UNTIL     IX    >        20
               MOVE      WK-FLCDT(IX)   TO       FILECD
               MOVE      WK-RTNMT(IX)   TO       ROUTENM
               MOVE      WK-RTCDT(IX)   TO       ROUTECD
               MOVE      WK-RTCNTT(IX)  TO       DATASU
         IF    WK-FLCDT(IX)   NOT =     ZERO
               EVALUATE  IX
                   WHEN   1
                   MOVE  SPACE         TO  K01-REC
                   INITIALIZE             K01-REC
                   MOVE  WK-RTCNTT(IX) TO  K01-F01
                   WRITE K01-REC
                   WHEN   2
                   MOVE  SPACE         TO  K02-REC
                   INITIALIZE             K02-REC
                   MOVE  WK-RTCNTT(IX) TO  K02-F01
                   WRITE K02-REC
                   WHEN   3
                   MOVE  SPACE         TO  K03-REC
                   INITIALIZE             K03-REC
                   MOVE  WK-RTCNTT(IX) TO  K03-F01
                   WRITE K03-REC
                   WHEN   4
                   MOVE  SPACE         TO  K04-REC
                   INITIALIZE             K04-REC
                   MOVE  WK-RTCNTT(IX) TO  K04-F01
                   WRITE K04-REC
                   WHEN   5
                   MOVE  SPACE         TO  K05-REC
                   INITIALIZE             K05-REC
                   MOVE  WK-RTCNTT(IX) TO  K05-F01
                   WRITE K05-REC
                   WHEN   6
                   MOVE  SPACE         TO  K06-REC
                   INITIALIZE             K06-REC
                   MOVE  WK-RTCNTT(IX) TO  K06-F01
                   WRITE K06-REC
                   WHEN   7
                   MOVE  SPACE         TO  K07-REC
                   INITIALIZE             K07-REC
                   MOVE  WK-RTCNTT(IX) TO  K07-F01
                   WRITE K07-REC
                   WHEN   8
                   MOVE  SPACE         TO  K08-REC
                   INITIALIZE             K08-REC
                   MOVE  WK-RTCNTT(IX) TO  K08-F01
                   WRITE K08-REC
                   WHEN   9
                   MOVE  SPACE         TO  K09-REC
                   INITIALIZE             K09-REC
                   MOVE  WK-RTCNTT(IX) TO  K09-F01
                   WRITE K09-REC
                   WHEN   10
                   MOVE  SPACE         TO  K10-REC
                   INITIALIZE             K10-REC
                   MOVE  WK-RTCNTT(IX) TO  K10-F01
                   WRITE K10-REC
                   WHEN   11
                   MOVE  SPACE         TO  K11-REC
                   INITIALIZE             K11-REC
                   MOVE  WK-RTCNTT(IX) TO  K11-F01
                   WRITE K11-REC
                   WHEN   12
                   MOVE  SPACE         TO  K12-REC
                   INITIALIZE             K12-REC
                   MOVE  WK-RTCNTT(IX) TO  K12-F01
                   WRITE K12-REC
                   WHEN   13
                   MOVE  SPACE         TO  K13-REC
                   INITIALIZE             K13-REC
                   MOVE  WK-RTCNTT(IX) TO  K13-F01
                   WRITE K13-REC
                   WHEN   14
                   MOVE  SPACE         TO  K14-REC
                   INITIALIZE             K14-REC
                   MOVE  WK-RTCNTT(IX) TO  K14-F01
                   WRITE K14-REC
                   WHEN   15
                   MOVE  SPACE         TO  K15-REC
                   INITIALIZE             K15-REC
                   MOVE  WK-RTCNTT(IX) TO  K15-F01
                   WRITE K15-REC
                   WHEN   16
                   MOVE  SPACE         TO  K16-REC
                   INITIALIZE             K16-REC
                   MOVE  WK-RTCNTT(IX) TO  K16-F01
                   WRITE K16-REC
                   WHEN   17
                   MOVE  SPACE         TO  K17-REC
                   INITIALIZE             K17-REC
                   MOVE  WK-RTCNTT(IX) TO  K17-F01
                   WRITE K17-REC
                   WHEN   18
                   MOVE  SPACE         TO  K18-REC
                   INITIALIZE             K18-REC
                   MOVE  WK-RTCNTT(IX) TO  K18-F01
                   WRITE K18-REC
                   WHEN   19
                   MOVE  SPACE         TO  K19-REC
                   INITIALIZE             K19-REC
                   MOVE  WK-RTCNTT(IX) TO  K19-F01
                   WRITE K19-REC
                   WHEN   20
                   MOVE  SPACE         TO  K20-REC
                   INITIALIZE             K20-REC
                   MOVE  WK-RTCNTT(IX) TO  K20-F01
                   WRITE K20-REC
               END-EVALUATE
***************WRITE     PRINT-REC FROM MEISAI   AFTER 1
***************WRITE     PRINT-REC FROM SEN2     AFTER 1
         END-IF
     END-PERFORM.
 LISTWT-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
