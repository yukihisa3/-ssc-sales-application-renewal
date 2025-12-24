# SKE0130B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKE0130B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷検品サブシステム              *
*    業務名　　　　　　　：　出荷検品                          *
*    モジュール名　　　　：　（手書）伝票データ送信全件        *
*    作成日／更新日　　　：　2000/10/13                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　売上伝票Ｆを読み、条件Ｆの出力Ｆ番*
*                            号のファイルへ出荷検品用データを抽*
*                            出する。                         *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE0130B.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          00/10/13.
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
*売上伝票データ
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL6
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F277  DEN-F274
                                            DEN-F09   DEN-F02
                                            DEN-F03
                        FILE      STATUS    IS   DEN-STATUS.
*条件ファイル
     SELECT   JYOKENF   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01
                                            JYO-F02
                        FILE  STATUS   IS   JYO-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
*送信用データ１
     SELECT   TEGDEN01  ASSIGN    TO        TEGDEN01
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D01-STATUS.
*送信用データ２
     SELECT   TEGDEN02  ASSIGN    TO        TEGDEN02
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D02-STATUS.
*送信用データ３
     SELECT   TEGDEN03  ASSIGN    TO        TEGDEN03
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D03-STATUS.
*送信用データ４
     SELECT   TEGDEN04  ASSIGN    TO        TEGDEN04
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D04-STATUS.
*送信用データ５
     SELECT   TEGDEN05  ASSIGN    TO        TEGDEN05
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D05-STATUS.
*送信用伝票データ６
     SELECT   TEGDEN06  ASSIGN    TO        TEGDEN06
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D06-STATUS.
*送信用伝票データ７
     SELECT   TEGDEN07  ASSIGN    TO        TEGDEN07
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D07-STATUS.
*送信用伝票データ８
     SELECT   TEGDEN08  ASSIGN    TO        TEGDEN08
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D08-STATUS.
*送信用伝票データ９
     SELECT   TEGDEN09  ASSIGN    TO        TEGDEN09
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D09-STATUS.
*送信用伝票データ１０
     SELECT   TEGDEN10  ASSIGN    TO        TEGDEN10
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D10-STATUS.
*送信用伝票データ１１
     SELECT   TEGDEN11  ASSIGN    TO        TEGDEN11
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D11-STATUS.
*送信用伝票データ１２
     SELECT   TEGDEN12  ASSIGN    TO        TEGDEN12
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D12-STATUS.
*送信用伝票データ１３
     SELECT   TEGDEN13  ASSIGN    TO        TEGDEN13
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D13-STATUS.
*送信用伝票データ１４
     SELECT   TEGDEN14  ASSIGN    TO        TEGDEN14
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D14-STATUS.
*送信用伝票データ１５
     SELECT   TEGDEN15  ASSIGN    TO        TEGDEN15
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D15-STATUS.
*送信用伝票データ１６
     SELECT   TEGDEN16  ASSIGN    TO        TEGDEN16
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D16-STATUS.
*送信用伝票データ１７
     SELECT   TEGDEN17  ASSIGN    TO        TEGDEN17
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D17-STATUS.
*送信用伝票データ１８
     SELECT   TEGDEN18  ASSIGN    TO        TEGDEN18
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D18-STATUS.
*送信用伝票データ１９
     SELECT   TEGDEN19  ASSIGN    TO        TEGDEN19
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D19-STATUS.
*送信用伝票データ２０
     SELECT   TEGDEN20  ASSIGN    TO        TEGDEN20
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D20-STATUS.
*件数ファイル０１
     SELECT   TEGDEK01  ASSIGN    TO        TEGDEK01
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K01-STATUS.
*件数ファイル０２
     SELECT   TEGDEK02  ASSIGN    TO        TEGDEK02
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K02-STATUS.
*件数ファイル０３
     SELECT   TEGDEK03  ASSIGN    TO        TEGDEK03
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K03-STATUS.
*件数ファイル０４
     SELECT   TEGDEK04  ASSIGN    TO        TEGDEK04
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K04-STATUS.
*件数ファイル０５
     SELECT   TEGDEK05  ASSIGN    TO        TEGDEK05
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K05-STATUS.
*件数ファイル０６
     SELECT   TEGDEK06  ASSIGN    TO        TEGDEK06
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K06-STATUS.
*件数ファイル０７
     SELECT   TEGDEK07  ASSIGN    TO        TEGDEK07
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K07-STATUS.
*件数ファイル０８
     SELECT   TEGDEK08  ASSIGN    TO        TEGDEK08
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K08-STATUS.
*件数ファイル０９
     SELECT   TEGDEK09  ASSIGN    TO        TEGDEK09
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K09-STATUS.
*件数ファイル１０
     SELECT   TEGDEK10  ASSIGN    TO        TEGDEK10
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K10-STATUS.
*件数ファイル１１
     SELECT   TEGDEK11  ASSIGN    TO        TEGDEK11
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K11-STATUS.
*件数ファイル１２
     SELECT   TEGDEK12  ASSIGN    TO        TEGDEK12
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K12-STATUS.
*件数ファイル１３
     SELECT   TEGDEK13  ASSIGN    TO        TEGDEK13
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K13-STATUS.
*件数ファイル１４
     SELECT   TEGDEK14  ASSIGN    TO        TEGDEK14
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K14-STATUS.
*件数ファイル１５
     SELECT   TEGDEK15  ASSIGN    TO        TEGDEK15
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K15-STATUS.
*件数ファイル１６
     SELECT   TEGDEK16  ASSIGN    TO        TEGDEK16
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K16-STATUS.
*件数ファイル１７
     SELECT   TEGDEK17  ASSIGN    TO        TEGDEK17
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K17-STATUS.
*件数ファイル１８
     SELECT   TEGDEK18  ASSIGN    TO        TEGDEK18
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K18-STATUS.
*件数ファイル１９
     SELECT   TEGDEK19  ASSIGN    TO        TEGDEK19
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K19-STATUS.
*件数ファイル２０
     SELECT   TEGDEK20  ASSIGN    TO        TEGDEK20
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K20-STATUS.
*プリント Ｆ
     SELECT      PRINTF      ASSIGN    TO        LP-04-PRTF.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    条件ファイル
******************************************************************
 FD  JYOKENF
                        LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO  AS   PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*
******************************************************************
*    送信用店舗データ１
******************************************************************
 FD  TEGDEN01           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D01       PREFIX.
******************************************************************
*    送信用店舗データ２
******************************************************************
 FD  TEGDEN02           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D02       PREFIX.
******************************************************************
*    送信用店舗データ３
******************************************************************
 FD  TEGDEN03           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D03       PREFIX.
******************************************************************
*    送信用店舗データ４
******************************************************************
 FD  TEGDEN04           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D04       PREFIX.
******************************************************************
*    送信用店舗データ５
******************************************************************
 FD  TEGDEN05           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D05       PREFIX.
******************************************************************
*    送信用店舗データ６
******************************************************************
 FD  TEGDEN06           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D06       PREFIX.
******************************************************************
*    送信用店舗データ７
******************************************************************
 FD  TEGDEN07           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D07       PREFIX.
******************************************************************
*    送信用店舗データ８
******************************************************************
 FD  TEGDEN08           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D08       PREFIX.
******************************************************************
*    送信用店舗データ９
******************************************************************
 FD  TEGDEN09           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D09       PREFIX.
******************************************************************
*    送信用店舗データ１０
******************************************************************
 FD  TEGDEN10           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D10       PREFIX.
******************************************************************
*    送信用店舗データ１１
******************************************************************
 FD  TEGDEN11           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D11       PREFIX.
******************************************************************
*    送信用店舗データ１２
******************************************************************
 FD  TEGDEN12           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D12       PREFIX.
******************************************************************
*    送信用店舗データ１３
******************************************************************
 FD  TEGDEN13           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D13       PREFIX.
******************************************************************
*    送信用店舗データ１４
******************************************************************
 FD  TEGDEN14           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D14       PREFIX.
******************************************************************
*    送信用店舗データ１５
******************************************************************
 FD  TEGDEN15           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D15       PREFIX.
******************************************************************
*    送信用店舗データ１６
******************************************************************
 FD  TEGDEN16           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D16       PREFIX.
******************************************************************
*    送信用店舗データ１７
******************************************************************
 FD  TEGDEN17           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D17       PREFIX.
******************************************************************
*    送信用店舗データ１８
******************************************************************
 FD  TEGDEN18           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D18       PREFIX.
******************************************************************
*    送信用店舗データ１９
******************************************************************
 FD  TEGDEN19           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D19       PREFIX.
******************************************************************
*    送信用店舗データ２０
******************************************************************
 FD  TEGDEN20           LABEL RECORD   IS   STANDARD.
     COPY     TEGDENF   OF        XFDLIB
              JOINING   D20       PREFIX.
******************************************************************
*    件数データ０１
******************************************************************
 FD  TEGDEK01           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K01-REC.
     03  K01-F01             PIC  9(08).
     03  K01-F02             PIC  X(02).
******************************************************************
*    件数データ０２
******************************************************************
 FD  TEGDEK02           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K02-REC.
     03  K02-F01             PIC  9(08).
     03  K02-F02             PIC  X(02).
******************************************************************
*    件数データ０３
******************************************************************
 FD  TEGDEK03           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K03-REC.
     03  K03-F01             PIC  9(08).
     03  K03-F02             PIC  X(02).
******************************************************************
*    件数データ０４
******************************************************************
 FD  TEGDEK04           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K04-REC.
     03  K04-F01             PIC  9(08).
     03  K04-F02             PIC  X(02).
******************************************************************
*    件数データ０５
******************************************************************
 FD  TEGDEK05           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K05-REC.
     03  K05-F01             PIC  9(08).
     03  K05-F02             PIC  X(02).
******************************************************************
*    件数データ０６
******************************************************************
 FD  TEGDEK06           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K06-REC.
     03  K06-F01             PIC  9(08).
     03  K06-F02             PIC  X(02).
******************************************************************
*    件数データ０７
******************************************************************
 FD  TEGDEK07           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K07-REC.
     03  K07-F01             PIC  9(08).
     03  K07-F02             PIC  X(02).
******************************************************************
*    件数データ０８
******************************************************************
 FD  TEGDEK08           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K08-REC.
     03  K08-F01             PIC  9(08).
     03  K08-F02             PIC  X(02).
******************************************************************
*    件数データ０９
******************************************************************
 FD  TEGDEK09           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K09-REC.
     03  K09-F01             PIC  9(08).
     03  K09-F02             PIC  X(02).
******************************************************************
*    件数データ１０
******************************************************************
 FD  TEGDEK10           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K10-REC.
     03  K10-F01             PIC  9(08).
     03  K10-F02             PIC  X(02).
******************************************************************
*    件数データ１１
******************************************************************
 FD  TEGDEK11           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K11-REC.
     03  K11-F01             PIC  9(08).
     03  K11-F02             PIC  X(02).
******************************************************************
*    件数データ１２
******************************************************************
 FD  TEGDEK12           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K12-REC.
     03  K12-F01             PIC  9(08).
     03  K12-F02             PIC  X(02).
******************************************************************
*    件数データ１３
******************************************************************
 FD  TEGDEK13           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K13-REC.
     03  K13-F01             PIC  9(08).
     03  K13-F02             PIC  X(02).
******************************************************************
*    件数データ１４
******************************************************************
 FD  TEGDEK14           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K14-REC.
     03  K14-F01             PIC  9(08).
     03  K14-F02             PIC  X(02).
******************************************************************
*    件数データ１５
******************************************************************
 FD  TEGDEK15           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K15-REC.
     03  K15-F01             PIC  9(08).
     03  K15-F02             PIC  X(02).
******************************************************************
*    件数データ１６
******************************************************************
 FD  TEGDEK16           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K16-REC.
     03  K16-F01             PIC  9(08).
     03  K16-F02             PIC  X(02).
******************************************************************
*    件数データ１７
******************************************************************
 FD  TEGDEK17           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K17-REC.
     03  K17-F01             PIC  9(08).
     03  K17-F02             PIC  X(02).
******************************************************************
*    件数データ１８
******************************************************************
 FD  TEGDEK18           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K18-REC.
     03  K18-F01             PIC  9(08).
     03  K18-F02             PIC  X(02).
******************************************************************
*    件数データ１９
******************************************************************
 FD  TEGDEK19           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K19-REC.
     03  K19-F01             PIC  9(08).
     03  K19-F02             PIC  X(02).
******************************************************************
*    件数データ２０
******************************************************************
 FD  TEGDEK20           BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K20-REC.
     03  K20-F01             PIC  9(08).
     03  K20-F02             PIC  X(02).
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
 01  SKIP-FLG                PIC  9(01)     VALUE  ZERO.
 01  WK-DENPYO               PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE.
         05  SYS-YY        PIC 9(02).
         05  SYS-MM        PIC 9(02).
         05  SYS-DD        PIC 9(02).
     03  SYS-DATEW.
         05  SYS-YY-W      PIC 9(04).
         05  SYS-MM-W      PIC 9(02).
         05  SYS-DD-W      PIC 9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
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
         05  ST-PG          PIC   X(08)  VALUE "SKE0130B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKE0130B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKE0130B".
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
       NC"【　出荷検品手伝票振分件数リスト　】"
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
*    コメント行
 01  KOMENTO            CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(30)  VALUE SPACE.
     03  FILLER                   PIC       N(11)  VALUE
         NC"今回のバッチ番号は、［".
     03  FILLER                   PIC       X(01)  VALUE SPACE.
     03  B-YY                     PIC       9(04).
     03  FILLER                   PIC       X(01)  VALUE "/".
     03  B-MM                     PIC       9(02).
     03  FILLER                   PIC       X(01)  VALUE "/".
     03  B-DD                     PIC       9(02).
     03  FILLER                   PIC       X(04)  VALUE SPACE.
     03  B-HH                     PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE ":".
     03  B-MN                     PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE SPACE.
     03  FILLER                   PIC       N(12)  VALUE
         NC"］です。確認して下さい。".
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
     COPY   TEGDENF  OF XFDLIB  JOINING   WK  AS   PREFIX.
*
*    数量編集
 01  WK-HEN                 PIC   9(09)V9(02).
 01  WK-HEN-R               REDEFINES   WK-HEN.
     03  WK-HEN-1           PIC   9(09).
     03  WK-HEN-2           PIC   9(02).
*    数量編集
 01  WK-HEN1.
     03  WK-HEN1-1          PIC   X(01).
     03  WK-HEN1-2          PIC   X(09).
     03  WK-HEN1-3          PIC   X(01).
     03  WK-HEN1-4          PIC   X(02).
*    日付編集
 01  WK-HEN-DATE.
     03  WK-HEN-DATE1       PIC   X(04).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE2       PIC   X(02).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE3       PIC   X(02).
*    時間編集
 01  WK-HEN-TIME.
     03  WK-HEN-TIME1       PIC   X(02).
     03  FILLER             PIC   X(01)  VALUE  ":".
     03  WK-HEN-TIME2       PIC   X(02).
*    日付変換１
 01  WK-HENKAN              PIC   9(08)  VALUE  ZERO.
 01  WK-HIDUKE.
     03  WK-HIDUKE1         PIC   9(04).
     03  WK-HIDUKE2         PIC   9(02).
     03  WK-HIDUKE3         PIC   9(02).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
 LINKAGE                SECTION.
 01  PARA-SOKO              PIC   X(02).
 01  PARA-TOKCD             PIC   9(08).
 01  PARA-SDEN              PIC   9(09).
 01  PARA-EDEN              PIC   9(09).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-SOKO
                                       PARA-TOKCD
                                       PARA-SDEN
                                       PARA-EDEN.
*
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN01.
     MOVE      "TEGDEN01"   TO   AB-FILE.
     MOVE      D01-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN02.
     MOVE      "TEGDEN02"   TO   AB-FILE.
     MOVE      D02-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN03.
     MOVE      "TEGDEN03"   TO   AB-FILE.
     MOVE      D03-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN04.
     MOVE      "TEGDEN04"   TO   AB-FILE.
     MOVE      D04-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN05.
     MOVE      "TEGDEN05"   TO   AB-FILE.
     MOVE      D05-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN06.
     MOVE      "TEGDEN06"   TO   AB-FILE.
     MOVE      D06-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN07.
     MOVE      "TEGDEN07"   TO   AB-FILE.
     MOVE      D07-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN08.
     MOVE      "TEGDEN08"   TO   AB-FILE.
     MOVE      D08-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN09.
     MOVE      "TEGDEN09"   TO   AB-FILE.
     MOVE      D09-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN10.
     MOVE      "TEGDEN10"   TO   AB-FILE.
     MOVE      D10-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN11.
     MOVE      "TEGDEN11"   TO   AB-FILE.
     MOVE      D11-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC12          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN12.
     MOVE      "TEGDEN12"   TO   AB-FILE.
     MOVE      D12-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC13          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN13.
     MOVE      "TEGDEN13"   TO   AB-FILE.
     MOVE      D13-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC14          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN14.
     MOVE      "TEGDEN14"   TO   AB-FILE.
     MOVE      D14-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC15          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN15.
     MOVE      "TEGDEN15"   TO   AB-FILE.
     MOVE      D15-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC16          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN16.
     MOVE      "TEGDEN16"   TO   AB-FILE.
     MOVE      D16-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC17          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN17.
     MOVE      "TEGDEN17"   TO   AB-FILE.
     MOVE      D17-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC18          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN18.
     MOVE      "TEGDEN18"   TO   AB-FILE.
     MOVE      D18-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC19          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN19.
     MOVE      "TEGDEN19"   TO   AB-FILE.
     MOVE      D19-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC20          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEN20.
     MOVE      "TEGDEN20"   TO   AB-FILE.
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
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENF  "   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC23          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK01.
     MOVE      "TEGDEK01"   TO   AB-FILE.
     MOVE      K01-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC24          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK02.
     MOVE      "TEGDEK02"   TO   AB-FILE.
     MOVE      K02-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC25          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK03.
     MOVE      "TEGDEK03"   TO   AB-FILE.
     MOVE      K03-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC26          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK04.
     MOVE      "TEGDEK04"   TO   AB-FILE.
     MOVE      K04-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC27          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK05.
     MOVE      "TEGDEK05"   TO   AB-FILE.
     MOVE      K05-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC28          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK06.
     MOVE      "TEGDEK06"   TO   AB-FILE.
     MOVE      K06-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC29          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK07.
     MOVE      "TEGDEK07"   TO   AB-FILE.
     MOVE      K07-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC30          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK08.
     MOVE      "TEGDEK08"   TO   AB-FILE.
     MOVE      K08-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC31          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK09.
     MOVE      "TEGDEK09"   TO   AB-FILE.
     MOVE      K09-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC32          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK10.
     MOVE      "TEGDEK10"   TO   AB-FILE.
     MOVE      K10-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC33          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK11.
     MOVE      "TEGDEK11"   TO   AB-FILE.
     MOVE      K11-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC34          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK12.
     MOVE      "TEGDEK12"   TO   AB-FILE.
     MOVE      K12-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC35          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK13.
     MOVE      "TEGDEK13"   TO   AB-FILE.
     MOVE      K13-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC36          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK14.
     MOVE      "TEGDEK14"   TO   AB-FILE.
     MOVE      K14-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC37          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK15.
     MOVE      "TEGDEK15"   TO   AB-FILE.
     MOVE      K15-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC38          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK16.
     MOVE      "TEGDEK16"   TO   AB-FILE.
     MOVE      K16-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC39          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK17.
     MOVE      "TEGDEK17"   TO   AB-FILE.
     MOVE      K17-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC40          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK18.
     MOVE      "TEGDEK18"   TO   AB-FILE.
     MOVE      K18-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC41          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK19.
     MOVE      "TEGDEK19"   TO   AB-FILE.
     MOVE      K19-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC42          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TEGDEK20.
     MOVE      "TEGDEK20"   TO   AB-FILE.
     MOVE      K20-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC43          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
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
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*    システム日付８桁変換
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8  TO   SYS-DATEW.
     OPEN     INPUT     JYOKENF SHTDENF SHOTBL1.
     OPEN     OUTPUT    TEGDEN01.
     OPEN     OUTPUT    TEGDEN02.
     OPEN     OUTPUT    TEGDEN03.
     OPEN     OUTPUT    TEGDEN04.
     OPEN     OUTPUT    TEGDEN05.
     OPEN     OUTPUT    TEGDEN06.
     OPEN     OUTPUT    TEGDEN07.
     OPEN     OUTPUT    TEGDEN08.
     OPEN     OUTPUT    TEGDEN09.
     OPEN     OUTPUT    TEGDEN10.
     OPEN     OUTPUT    TEGDEN11.
     OPEN     OUTPUT    TEGDEN12.
     OPEN     OUTPUT    TEGDEN13.
     OPEN     OUTPUT    TEGDEN14.
     OPEN     OUTPUT    TEGDEN15.
     OPEN     OUTPUT    TEGDEN16.
     OPEN     OUTPUT    TEGDEN17.
     OPEN     OUTPUT    TEGDEN18.
     OPEN     OUTPUT    TEGDEN19.
     OPEN     OUTPUT    TEGDEN20.
     OPEN     OUTPUT    TEGDEK01.
     OPEN     OUTPUT    TEGDEK02.
     OPEN     OUTPUT    TEGDEK03.
     OPEN     OUTPUT    TEGDEK04.
     OPEN     OUTPUT    TEGDEK05.
     OPEN     OUTPUT    TEGDEK06.
     OPEN     OUTPUT    TEGDEK07.
     OPEN     OUTPUT    TEGDEK08.
     OPEN     OUTPUT    TEGDEK09.
     OPEN     OUTPUT    TEGDEK10.
     OPEN     OUTPUT    TEGDEK11.
     OPEN     OUTPUT    TEGDEK12.
     OPEN     OUTPUT    TEGDEK13.
     OPEN     OUTPUT    TEGDEK14.
     OPEN     OUTPUT    TEGDEK15.
     OPEN     OUTPUT    TEGDEK16.
     OPEN     OUTPUT    TEGDEK17.
     OPEN     OUTPUT    TEGDEK18.
     OPEN     OUTPUT    TEGDEK19.
     OPEN     OUTPUT    TEGDEK20.
     OPEN     OUTPUT    PRINTF.
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
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     ZERO           TO   DEN-F277.
     MOVE     ZERO           TO   DEN-F274.
     MOVE     PARA-SOKO      TO   DEN-F09.
     MOVE     PARA-SDEN      TO   DEN-F02.
     MOVE     ZERO           TO   DEN-F03.
     START    SHTDENF   KEY  >=   DEN-F277  DEN-F274
                                  DEN-F09   DEN-F02
                                  DEN-F03
         INVALID   KEY
              MOVE   "END"   TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*    売上伝票Ｆ初期読み
     PERFORM     SHTDENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    出力ファイル番号取得
     PERFORM  JYOKENF-READ-SEC.
*    売上伝票情報セット
     PERFORM URIAGE-SET-SEC
*    ファイル出力テーブル判定
     PERFORM TBLSET-SEC
*    売上伝票Ｆ読込み
     PERFORM  SHTDENF-READ-SEC.
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
     CLOSE     JYOKENF  SHTDENF  SHOTBL1.
     CLOSE     TEGDEN01.
     CLOSE     TEGDEN02.
     CLOSE     TEGDEN03.
     CLOSE     TEGDEN04.
     CLOSE     TEGDEN05.
     CLOSE     TEGDEN06.
     CLOSE     TEGDEN07.
     CLOSE     TEGDEN08.
     CLOSE     TEGDEN09.
     CLOSE     TEGDEN10.
     CLOSE     TEGDEN11.
     CLOSE     TEGDEN12.
     CLOSE     TEGDEN13.
     CLOSE     TEGDEN14.
     CLOSE     TEGDEN15.
     CLOSE     TEGDEN16.
     CLOSE     TEGDEN17.
     CLOSE     TEGDEN18.
     CLOSE     TEGDEN19.
     CLOSE     TEGDEN20.
     CLOSE     TEGDEK01.
     CLOSE     TEGDEK02.
     CLOSE     TEGDEK03.
     CLOSE     TEGDEK04.
     CLOSE     TEGDEK05.
     CLOSE     TEGDEK06.
     CLOSE     TEGDEK07.
     CLOSE     TEGDEK08.
     CLOSE     TEGDEK09.
     CLOSE     TEGDEK10.
     CLOSE     TEGDEK11.
     CLOSE     TEGDEK12.
     CLOSE     TEGDEK13.
     CLOSE     TEGDEK14.
     CLOSE     TEGDEK15.
     CLOSE     TEGDEK16.
     CLOSE     TEGDEK17.
     CLOSE     TEGDEK18.
     CLOSE     TEGDEK19.
     CLOSE     TEGDEK20.
     CLOSE     PRINTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　倉庫出力先ファイル判定→ファイル出力            *
****************************************************************
 URIAGE-SET-SEC         SECTION.
*
     MOVE     "KENPIN-SET-SEC"  TO      S-NAME.
*
*送信用伝票データ出力
     MOVE     SPACE          TO   WK-REC.
     INITIALIZE                   WK-REC.
*取引先ＣＤ
     MOVE     DEN-F01        TO   WK-F01.
*伝票番号
     MOVE     DEN-F02        TO   WK-F02.
*行番号
     MOVE     DEN-F03        TO   WK-F03.
*店舗ＣＤ
     MOVE     DEN-F07        TO   WK-F04.
*注文日
     MOVE     DEN-F111       TO   WK-HENKAN.
     MOVE     WK-HENKAN      TO   WK-HIDUKE.
     MOVE     WK-HIDUKE1     TO   WK-HEN-DATE1.
     MOVE     WK-HIDUKE2     TO   WK-HEN-DATE2.
     MOVE     WK-HIDUKE3     TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE    TO   WK-F05.
*納品日
     MOVE     DEN-F112       TO   WK-HENKAN.
     MOVE     WK-HENKAN      TO   WK-HIDUKE.
     MOVE     WK-HIDUKE1     TO   WK-HEN-DATE1.
     MOVE     WK-HIDUKE2     TO   WK-HEN-DATE2.
     MOVE     WK-HIDUKE3     TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE    TO   WK-F06.
*分類ＣＤ
     MOVE     DEN-F12        TO   WK-F07.
*自社商品ＣＤ
     MOVE     DEN-F141       TO   WK-F08.
*商品名称１
     MOVE     DEN-F1421      TO   WK-F09.
*商品名称２
     MOVE     DEN-F1422      TO   WK-F10.
*数量
     INITIALIZE                   WK-HEN1.
     IF       DEN-F15  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F15        TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   WK-F11.
*原価単価
     INITIALIZE                   WK-HEN1.
     IF       DEN-F172  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F172       TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   WK-F12.
*売価単価
     INITIALIZE                   WK-HEN1.
     IF       DEN-F173  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F173       TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   WK-F13.
*指定商品ＣＤ（取引先商品ＣＤ）
     MOVE     DEN-F25        TO   WK-F14.
*オンライン区分
     MOVE     DEN-F274       TO   WK-F15.
*店舗名称（カナ）
     MOVE     DEN-F30        TO   WK-F16.
*受信日
     MOVE     SYS-DATEW      TO   WK-HENKAN.
     MOVE     WK-HENKAN      TO   WK-HIDUKE.
     MOVE     WK-HIDUKE1     TO   WK-HEN-DATE1.
     MOVE     WK-HIDUKE2     TO   WK-HEN-DATE2.
     MOVE     WK-HIDUKE3     TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE    TO   WK-F17.
*受信時間
     MOVE     SYS-HH         TO   WK-HEN-TIME1.
     MOVE     SYS-MN         TO   WK-HEN-TIME2.
     MOVE     WK-HEN-TIME    TO   WK-F18.
*振分倉庫ＣＤ
     MOVE     DEN-F09        TO   WK-F19.
*_番
     MOVE     DEN-F49        TO   WK-F20.
*訂正前数量
     INITIALIZE                   WK-HEN1.
     IF       DEN-F50  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F50        TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   WK-F21.
*取引先ＣＤセット
     IF       DEN-F132  =  "99"
              MOVE  "000760" TO   WK-F22(40:6)
     ELSE
              MOVE  "000173" TO   WK-F22(40:6)
     END-IF.
*改行コード
     MOVE     X"0D0A"        TO   WK-F23.
*  商品変換テーブル検索
     MOVE     DEN-F01        TO   TBL-F01.
     MOVE     DEN-F25        TO   TBL-F02.
     READ     SHOTBL1
       INVALID
              CONTINUE
       NOT  INVALID
              MOVE TBL-F08   TO   WK-F20
     END-READ.
*
 URIAGE-SET-EXIT.
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
     MOVE      DEN-F09   TO      JYO-F02.
*****DISPLAY "DEN-F09 = " DEN-F09 UPON CONS.
     READ      JYOKENF   INVALID
               DISPLAY "JYOKENF INVALID KEY = "
                        JYO-F01 ":" JYO-F02  UPON CONS
               STOP  RUN
       NOT INVALID
              EVALUATE  JYO-F04
                 WHEN   1   MOVE  JYO-F04  TO  WK-FLCD1
                            MOVE  DEN-F48  TO  WK-RTCD1
                            MOVE  JYO-F03  TO  WK-RTNM1
                 WHEN   2   MOVE  JYO-F04  TO  WK-FLCD2
                            MOVE  DEN-F48  TO  WK-RTCD2
                            MOVE  JYO-F03  TO  WK-RTNM2
                 WHEN   3   MOVE  JYO-F04  TO  WK-FLCD3
                            MOVE  DEN-F48  TO  WK-RTCD3
                            MOVE  JYO-F03  TO  WK-RTNM3
                 WHEN   4   MOVE  JYO-F04  TO  WK-FLCD4
                            MOVE  DEN-F48  TO  WK-RTCD4
                            MOVE  JYO-F03  TO  WK-RTNM4
                 WHEN   5   MOVE  JYO-F04  TO  WK-FLCD5
                            MOVE  DEN-F48  TO  WK-RTCD5
                            MOVE  JYO-F03  TO  WK-RTNM5
                 WHEN   6   MOVE  JYO-F04  TO  WK-FLCD6
                            MOVE  DEN-F48  TO  WK-RTCD6
                            MOVE  JYO-F03  TO  WK-RTNM6
                 WHEN   7   MOVE  JYO-F04  TO  WK-FLCD7
                            MOVE  DEN-F48  TO  WK-RTCD7
                            MOVE  JYO-F03  TO  WK-RTNM7
                 WHEN   8   MOVE  JYO-F04  TO  WK-FLCD8
                            MOVE  DEN-F48  TO  WK-RTCD8
                            MOVE  JYO-F03  TO  WK-RTNM8
                 WHEN   9   MOVE  JYO-F04  TO  WK-FLCD9
                            MOVE  DEN-F48  TO  WK-RTCD9
                            MOVE  JYO-F03  TO  WK-RTNM9
                 WHEN  10   MOVE  JYO-F04  TO  WK-FLCD10
                            MOVE  DEN-F48  TO  WK-RTCD10
                            MOVE  JYO-F03  TO  WK-RTNM10
                 WHEN  11   MOVE  JYO-F04  TO  WK-FLCD11
                            MOVE  DEN-F48  TO  WK-RTCD11
                            MOVE  JYO-F03  TO  WK-RTNM11
                 WHEN  12   MOVE  JYO-F04  TO  WK-FLCD12
                            MOVE  DEN-F48  TO  WK-RTCD12
                            MOVE  JYO-F03  TO  WK-RTNM12
                 WHEN  13   MOVE  JYO-F04  TO  WK-FLCD13
                            MOVE  DEN-F48  TO  WK-RTCD13
                            MOVE  JYO-F03  TO  WK-RTNM13
                 WHEN  14   MOVE  JYO-F04  TO  WK-FLCD14
                            MOVE  DEN-F48  TO  WK-RTCD14
                            MOVE  JYO-F03  TO  WK-RTNM14
                 WHEN  15   MOVE  JYO-F04  TO  WK-FLCD15
                            MOVE  DEN-F48  TO  WK-RTCD15
                            MOVE  JYO-F03  TO  WK-RTNM15
                 WHEN  16   MOVE  JYO-F04  TO  WK-FLCD16
                            MOVE  DEN-F48  TO  WK-RTCD16
                            MOVE  JYO-F03  TO  WK-RTNM16
                 WHEN  17   MOVE  JYO-F04  TO  WK-FLCD17
                            MOVE  DEN-F48  TO  WK-RTCD17
                            MOVE  JYO-F03  TO  WK-RTNM17
                 WHEN  18   MOVE  JYO-F04  TO  WK-FLCD18
                            MOVE  DEN-F48  TO  WK-RTCD18
                            MOVE  JYO-F03  TO  WK-RTNM18
                 WHEN  19   MOVE  JYO-F04  TO  WK-FLCD19
                            MOVE  DEN-F48  TO  WK-RTCD19
                            MOVE  JYO-F03  TO  WK-RTNM19
                 WHEN  20   MOVE  JYO-F04  TO  WK-FLCD20
                            MOVE  DEN-F48  TO  WK-RTCD20
                            MOVE  JYO-F03  TO  WK-RTNM20
              END-EVALUATE
     END-READ.
*
 JYOKENF-READ-EXIT.
     EXIT.
****************************************************************
*           リスト出力処理                          3.1.1      *
****************************************************************
 LISTWT-SEC   SECTION.
*
     MOVE      SYS-YY         TO        YY.
     MOVE      SYS-MM         TO        MM.
     MOVE      SYS-DD         TO        DD.
     MOVE      1              TO        PEIJI.
     WRITE     PRINT-REC      FROM      MIDASHI1 AFTER 2.
     WRITE     PRINT-REC      FROM      SEN1     AFTER 2.
     WRITE     PRINT-REC      FROM      MIDASHI2 AFTER 1.
     WRITE     PRINT-REC      FROM      SEN1     AFTER 1.
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
                   MOVE  X"0D0A"       TO   K01-F02
                   WRITE K01-REC
                   WHEN   2
                   MOVE  SPACE         TO  K02-REC
                   INITIALIZE             K02-REC
                   MOVE  WK-RTCNTT(IX) TO  K02-F01
                   MOVE  X"0D0A"       TO   K02-F02
                   WRITE K02-REC
                   WHEN   3
                   MOVE  SPACE         TO  K03-REC
                   INITIALIZE             K03-REC
                   MOVE  WK-RTCNTT(IX) TO  K03-F01
                   MOVE  X"0D0A"       TO   K03-F02
                   WRITE K03-REC
                   WHEN   4
                   MOVE  SPACE         TO  K04-REC
                   INITIALIZE             K04-REC
                   MOVE  WK-RTCNTT(IX) TO  K04-F01
                   MOVE  X"0D0A"       TO   K04-F02
                   WRITE K04-REC
                   WHEN   5
                   MOVE  SPACE         TO  K05-REC
                   INITIALIZE             K05-REC
                   MOVE  WK-RTCNTT(IX) TO  K05-F01
                   MOVE  X"0D0A"       TO   K05-F02
                   WRITE K05-REC
                   WHEN   6
                   MOVE  SPACE         TO  K06-REC
                   INITIALIZE             K06-REC
                   MOVE  WK-RTCNTT(IX) TO  K06-F01
                   MOVE  X"0D0A"       TO   K06-F02
                   WRITE K06-REC
                   WHEN   7
                   MOVE  SPACE         TO  K07-REC
                   INITIALIZE             K07-REC
                   MOVE  WK-RTCNTT(IX) TO  K07-F01
                   MOVE  X"0D0A"       TO   K07-F02
                   WRITE K07-REC
                   WHEN   8
                   MOVE  SPACE         TO  K08-REC
                   INITIALIZE             K08-REC
                   MOVE  WK-RTCNTT(IX) TO  K08-F01
                   MOVE  X"0D0A"       TO   K08-F02
                   WRITE K08-REC
                   WHEN   9
                   MOVE  SPACE         TO  K09-REC
                   INITIALIZE             K09-REC
                   MOVE  WK-RTCNTT(IX) TO  K09-F01
                   MOVE  X"0D0A"       TO   K09-F02
                   WRITE K09-REC
                   WHEN   10
                   MOVE  SPACE         TO  K10-REC
                   INITIALIZE             K10-REC
                   MOVE  WK-RTCNTT(IX) TO  K10-F01
                   MOVE  X"0D0A"       TO   K10-F02
                   WRITE K10-REC
                   WHEN   11
                   MOVE  SPACE         TO  K11-REC
                   INITIALIZE             K11-REC
                   MOVE  WK-RTCNTT(IX) TO  K11-F01
                   MOVE  X"0D0A"       TO   K11-F02
                   WRITE K11-REC
                   WHEN   12
                   MOVE  SPACE         TO  K12-REC
                   INITIALIZE             K12-REC
                   MOVE  WK-RTCNTT(IX) TO  K12-F01
                   MOVE  X"0D0A"       TO   K12-F02
                   WRITE K12-REC
                   WHEN   13
                   MOVE  SPACE         TO  K13-REC
                   INITIALIZE             K13-REC
                   MOVE  WK-RTCNTT(IX) TO  K13-F01
                   MOVE  X"0D0A"       TO   K13-F02
                   WRITE K13-REC
                   WHEN   14
                   MOVE  SPACE         TO  K14-REC
                   INITIALIZE             K14-REC
                   MOVE  WK-RTCNTT(IX) TO  K14-F01
                   MOVE  X"0D0A"       TO   K14-F02
                   WRITE K14-REC
                   WHEN   15
                   MOVE  SPACE         TO  K15-REC
                   INITIALIZE             K15-REC
                   MOVE  WK-RTCNTT(IX) TO  K15-F01
                   MOVE  X"0D0A"       TO   K15-F02
                   WRITE K15-REC
                   WHEN   16
                   MOVE  SPACE         TO  K16-REC
                   INITIALIZE             K16-REC
                   MOVE  WK-RTCNTT(IX) TO  K16-F01
                   MOVE  X"0D0A"       TO   K16-F02
                   WRITE K16-REC
                   WHEN   17
                   MOVE  SPACE         TO  K17-REC
                   INITIALIZE             K17-REC
                   MOVE  WK-RTCNTT(IX) TO  K17-F01
                   MOVE  X"0D0A"       TO   K17-F02
                   WRITE K17-REC
                   WHEN   18
                   MOVE  SPACE         TO  K18-REC
                   INITIALIZE             K18-REC
                   MOVE  WK-RTCNTT(IX) TO  K18-F01
                   MOVE  X"0D0A"       TO   K18-F02
                   WRITE K18-REC
                   WHEN   19
                   MOVE  SPACE         TO  K19-REC
                   INITIALIZE             K19-REC
                   MOVE  WK-RTCNTT(IX) TO  K19-F01
                   MOVE  X"0D0A"       TO   K19-F02
                   WRITE K19-REC
                   WHEN   20
                   MOVE  SPACE         TO  K20-REC
                   INITIALIZE             K20-REC
                   MOVE  WK-RTCNTT(IX) TO  K20-F01
                   MOVE  X"0D0A"       TO   K20-F02
                   WRITE K20-REC
               END-EVALUATE
               WRITE     PRINT-REC FROM MEISAI   AFTER 1
               WRITE     PRINT-REC FROM SEN2     AFTER 1
         END-IF
     END-PERFORM.
     MOVE      SYS-YY-W         TO      B-YY.
     MOVE      SYS-MM-W         TO      B-MM.
     MOVE      SYS-DD-W         TO      B-DD.
     MOVE      SYS-HH           TO      B-HH.
     MOVE      SYS-MN           TO      B-MN.
     WRITE     PRINT-REC      FROM      KOMENTO  AFTER 2.
 LISTWT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SHTDENF-READ-SEC      SECTION.
*
     MOVE "SHTDENF-READ-SEC"   TO   S-NAME.
*
     READ     SHTDENF
              NEXT AT END
                 MOVE   "END"  TO   END-FLG
                 GO            TO   SHTDENF-READ-EXIT
              NOT AT END
                 ADD     1     TO   RD-CNT
     END-READ.
*終了条件
*パラ倉庫ＣＤ
     IF  DEN-F09        NOT  =    PARA-SOKO
         MOVE     "END"      TO   END-FLG
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-EXIT
     END-IF.
*パラ取引先ＣＤ
     IF  DEN-F01        NOT  =    PARA-TOKCD
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*伝票番号範囲チェック
     IF  DEN-F02             <    PARA-SDEN
     OR  DEN-F02             >    PARA-EDEN
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*売上作成ＦＬＧ
     IF  DEN-F277            =    9
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*オンライン
     IF  DEN-F274       NOT  =    0
         MOVE      "END"     TO   END-FLG
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-EXIT
     END-IF.
*読み飛ばし条件
*行_（８０の場合）
     IF  DEN-F03        =    80
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*行_（９０の場合）
     IF  DEN-F03        =    90
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*相殺区分
     IF  DEN-F04        NOT  =    0
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*伝区
     IF  DEN-F051       NOT  =    40
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*伝発
     IF  DEN-F134       NOT  =    0
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*倉庫ＣＤが未設定の場合、読み飛ばし
     IF  DEN-F09        =    SPACE
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*振分対象伝票番号退避／売上フラグチェック
     MOVE    DEN-F02    TO   WK-DENPYO.
     MOVE    1          TO   SKIP-FLG.
*
 SHTDENF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
