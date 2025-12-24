****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　オンラインデータ抽出処理　　　　　*
*    作成日／更新日　　　：　99/09/22                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取ったパラメタのバッチNO.     *
*                            倉庫コードより該当のデータを      *
*                            抽出する。                        *
*　　　　　　　　（全件数バージョン）　　　　　　　　　　　　　*
*    更新日／更新者　　　：　11/10/05 / YOSHIDA.M              *
*    修正概要　　　　　　：　基幹サーバ統合                    *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY0022B.
 AUTHOR.                NAV T.TAKAHASHI.
 DATE-WRITTEN.          99/09/13.
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
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
***2011.10.05(DEN-F07,DEN-F112)
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
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
                        FILE STATUS    IS   TBL-STATUS.
*送信用伝票データ１
     SELECT   JHSDEN01  ASSIGN    TO        JHSDEN01
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D01-STATUS.
*送信用伝票データ２
     SELECT   JHSDEN02  ASSIGN    TO        JHSDEN02
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D02-STATUS.
*送信用伝票データ３
     SELECT   JHSDEN03  ASSIGN    TO        JHSDEN03
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D03-STATUS.
*送信用伝票データ４
     SELECT   JHSDEN04  ASSIGN    TO        JHSDEN04
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D04-STATUS.
*送信用伝票データ５
     SELECT   JHSDEN05  ASSIGN    TO        JHSDEN05
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D05-STATUS.
*送信用伝票データ６
     SELECT   JHSDEN06  ASSIGN    TO        JHSDEN06
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D06-STATUS.
*送信用伝票データ７
     SELECT   JHSDEN07  ASSIGN    TO        JHSDEN07
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D07-STATUS.
*送信用伝票データ８
     SELECT   JHSDEN08  ASSIGN    TO        JHSDEN08
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D08-STATUS.
*送信用伝票データ９
     SELECT   JHSDEN09  ASSIGN    TO        JHSDEN09
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D09-STATUS.
*送信用伝票データ１０
     SELECT   JHSDEN10  ASSIGN    TO        JHSDEN10
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D10-STATUS.
*送信用伝票データ１１
     SELECT   JHSDEN11  ASSIGN    TO        JHSDEN11
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D11-STATUS.
*送信用伝票データ１２
     SELECT   JHSDEN12  ASSIGN    TO        JHSDEN12
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D12-STATUS.
*送信用伝票データ１３
     SELECT   JHSDEN13  ASSIGN    TO        JHSDEN13
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D13-STATUS.
*送信用伝票データ１４
     SELECT   JHSDEN14  ASSIGN    TO        JHSDEN14
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D14-STATUS.
*送信用伝票データ１５
     SELECT   JHSDEN15  ASSIGN    TO        JHSDEN15
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D15-STATUS.
*送信用伝票データ１６
     SELECT   JHSDEN16  ASSIGN    TO        JHSDEN16
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D16-STATUS.
*送信用伝票データ１７
     SELECT   JHSDEN17  ASSIGN    TO        JHSDEN17
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D17-STATUS.
*送信用伝票データ１８
     SELECT   JHSDEN18  ASSIGN    TO        JHSDEN18
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D18-STATUS.
*送信用伝票データ１９
     SELECT   JHSDEN19  ASSIGN    TO        JHSDEN19
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D19-STATUS.
*送信用伝票データ２０
     SELECT   JHSDEN20  ASSIGN    TO        JHSDEN20
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D20-STATUS.
*****<<  プリント Ｆ  >>**************************************
     SELECT      PRINTF      ASSIGN    TO        LP-04-PRTF.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLA
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
*
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*
******************************************************************
*    送信用売上伝票データ１
******************************************************************
 FD  JHSDEN01           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D01       PREFIX.
******************************************************************
*    送信用売上伝票データ２
******************************************************************
 FD  JHSDEN02           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D02       PREFIX.
******************************************************************
*    送信用売上伝票データ３
******************************************************************
 FD  JHSDEN03           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D03       PREFIX.
******************************************************************
*    送信用売上伝票データ４
******************************************************************
 FD  JHSDEN04           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D04       PREFIX.
******************************************************************
*    送信用売上伝票データ５
******************************************************************
 FD  JHSDEN05           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D05       PREFIX.
******************************************************************
*    送信用売上伝票データ６
******************************************************************
 FD  JHSDEN06           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D06       PREFIX.
******************************************************************
*    送信用売上伝票データ７
******************************************************************
 FD  JHSDEN07           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D07       PREFIX.
******************************************************************
*    送信用売上伝票データ８
******************************************************************
 FD  JHSDEN08           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D08       PREFIX.
******************************************************************
*    送信用売上伝票データ９
******************************************************************
 FD  JHSDEN09           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D09       PREFIX.
******************************************************************
*    送信用売上伝票データ１０
******************************************************************
 FD  JHSDEN10           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D10       PREFIX.
******************************************************************
*    送信用売上伝票データ１１
******************************************************************
 FD  JHSDEN11           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D11       PREFIX.
******************************************************************
*    送信用売上伝票データ１２
******************************************************************
 FD  JHSDEN12           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D12       PREFIX.
******************************************************************
*    送信用売上伝票データ１３
******************************************************************
 FD  JHSDEN13           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D13       PREFIX.
******************************************************************
*    送信用売上伝票データ１４
******************************************************************
 FD  JHSDEN14           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D14       PREFIX.
******************************************************************
*    送信用売上伝票データ１５
******************************************************************
 FD  JHSDEN15           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D15       PREFIX.
******************************************************************
*    送信用売上伝票データ１６
******************************************************************
 FD  JHSDEN16           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D16       PREFIX.
******************************************************************
*    送信用売上伝票データ１７
******************************************************************
 FD  JHSDEN17           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D17       PREFIX.
******************************************************************
*    送信用売上伝票データ１８
******************************************************************
 FD  JHSDEN18           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D18       PREFIX.
******************************************************************
*    送信用売上伝票データ１９
******************************************************************
 FD  JHSDEN19           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D19       PREFIX.
******************************************************************
*    送信用売上伝票データ２０
******************************************************************
 FD  JHSDEN20           LABEL RECORD   IS   STANDARD.
     COPY     JHSDENF   OF        XFDLIB
              JOINING   D20       PREFIX.
****************************************************************
*    FILE = プリント　ファイル                                 *
****************************************************************
 FD  PRINTF.
 01  PRINT-REC                    PIC       X(200).
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  IX                      PIC  9(02)     VALUE  ZERO.
*
 01  TANABAN-SAV-WORK.
     03  WK-TANABAN          PIC  X(06)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE.
         05  SYS-YY        PIC 9(02).
         05  SYS-MM        PIC 9(02).
         05  SYS-DD        PIC 9(02).
     03  SYS-DATEW         PIC 9(08).
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
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY0022B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY0022B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY0022B".
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
         05  WK-FLCDT        PIC  9(05)  OCCURS 20.
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
     03  FILLER                   PIC       X(38)  VALUE SPACE.
     03  FILLER                   PIC       N(17)  VALUE
       NC"【　伝票発行場所振分件数リスト　】"
       CHARACTER   TYPE   IS   YB-21.
     03  FILLER                   PIC       X(18)  VALUE SPACE.
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
*
*    見出し行１．５
 01  MIDASHI15                    CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC       X(38)  VALUE SPACE.
     03  FILLER                   PIC       N(07)  VALUE
       NC"【　バッチ_：".
     03  FILLER                   PIC       X(01)  VALUE SPACE.
     03  JYUSIN-DT                PIC       9(08).
     03  FILLER                   PIC       X(03)  VALUE " - ".
     03  JYUSIN-TM                PIC       9(08).
     03  FILLER                   PIC       X(03)  VALUE " - ".
     03  JYUSIN-TOKCD             PIC       9(08).
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
       NC"発行場所名".
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
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLA.
     MOVE      "SHTDENLA"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN01.
     MOVE      "JHSDEN01"   TO   AB-FILE.
     MOVE      D01-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN02.
     MOVE      "JHSDEN02"   TO   AB-FILE.
     MOVE      D02-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN03.
     MOVE      "JHSDEN03"   TO   AB-FILE.
     MOVE      D03-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN04.
     MOVE      "JHSDEN04"   TO   AB-FILE.
     MOVE      D04-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN05.
     MOVE      "JHSDEN05"   TO   AB-FILE.
     MOVE      D05-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN06.
     MOVE      "JHSDEN06"   TO   AB-FILE.
     MOVE      D06-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN07.
     MOVE      "JHSDEN07"   TO   AB-FILE.
     MOVE      D07-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN08.
     MOVE      "JHSDEN08"   TO   AB-FILE.
     MOVE      D08-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN09.
     MOVE      "JHSDEN09"   TO   AB-FILE.
     MOVE      D09-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN10.
     MOVE      "JHSDEN10"   TO   AB-FILE.
     MOVE      D10-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC12          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN11.
     MOVE      "JHSDEN11"   TO   AB-FILE.
     MOVE      D11-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC13          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN12.
     MOVE      "JHSDEN12"   TO   AB-FILE.
     MOVE      D12-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC14          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN13.
     MOVE      "JHSDEN13"   TO   AB-FILE.
     MOVE      D13-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC15          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN14.
     MOVE      "JHSDEN14"   TO   AB-FILE.
     MOVE      D14-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC16          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN15.
     MOVE      "JHSDEN15"   TO   AB-FILE.
     MOVE      D15-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC17          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN16.
     MOVE      "JHSDEN16"   TO   AB-FILE.
     MOVE      D16-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC18          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN17.
     MOVE      "JHSDEN17"   TO   AB-FILE.
     MOVE      D17-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC19          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN18.
     MOVE      "JHSDEN18"   TO   AB-FILE.
     MOVE      D18-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC20          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN19.
     MOVE      "JHSDEN19"   TO   AB-FILE.
     MOVE      D19-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC21          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSDEN20.
     MOVE      "JHSDEN20"   TO   AB-FILE.
     MOVE      D20-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC22          SECTION.
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
 FILEERR-SEC23          SECTION.
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
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHTDENLA JYOKENF SHOTBL1.
     OPEN     OUTPUT    JHSDEN01.
     OPEN     OUTPUT    JHSDEN02.
     OPEN     OUTPUT    JHSDEN03.
     OPEN     OUTPUT    JHSDEN04.
     OPEN     OUTPUT    JHSDEN05.
     OPEN     OUTPUT    JHSDEN06.
     OPEN     OUTPUT    JHSDEN07.
     OPEN     OUTPUT    JHSDEN08.
     OPEN     OUTPUT    JHSDEN09.
     OPEN     OUTPUT    JHSDEN10.
     OPEN     OUTPUT    JHSDEN11.
     OPEN     OUTPUT    JHSDEN12.
     OPEN     OUTPUT    JHSDEN13.
     OPEN     OUTPUT    JHSDEN14.
     OPEN     OUTPUT    JHSDEN15.
     OPEN     OUTPUT    JHSDEN16.
     OPEN     OUTPUT    JHSDEN17.
     OPEN     OUTPUT    JHSDEN18.
     OPEN     OUTPUT    JHSDEN19.
     OPEN     OUTPUT    JHSDEN20.
     OPEN     OUTPUT    PRINTF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT.
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
     MOVE     PARA-JDATE     TO   DEN-F46.
     MOVE     PARA-JTIME     TO   DEN-F47.
     MOVE     PARA-TORICD    TO   DEN-F01.
*****MOVE     1              TO   DEN-F48.
     MOVE     SPACE          TO   DEN-F48.
     MOVE     ZERO           TO   DEN-F02.
     MOVE     ZERO           TO   DEN-F04.
     MOVE     ZERO           TO   DEN-F051.
     MOVE     ZERO           TO   DEN-F03.
     START    SHTDENLA  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
***2011.10.05(DEN-F07,DEN-F112)
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     READ     SHTDENLA
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1    TO   RD-CNT
     END-READ.
     DISPLAY "DEN-F01 = " DEN-F01  UPON CONS.
     DISPLAY "DEN-F02 = " DEN-F02  UPON CONS.
     DISPLAY "DEN-F03 = " DEN-F03  UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     IF     ( PARA-JDATE     =    DEN-F46 ) AND
            ( PARA-JTIME     =    DEN-F47 ) AND
            ( PARA-TORICD    =    DEN-F01 )
              CONTINUE
     ELSE
              MOVE      9         TO   END-FG
              GO        TO        MAIN-EXIT
     END-IF.
*最新_番取得
     PERFORM  HSHOTBL-READ-SEC.
*出力ファイル番号取得
     PERFORM  JYOKENF-READ-SEC.
*送信用伝票データ出力
     EVALUATE JYO-F04
         WHEN 1
              MOVE     SPACE          TO   D01-REC
              INITIALIZE                   D01-REC
              MOVE     DEN-REC        TO   D01-REC
              MOVE     WK-TANABAN     TO   D01-F49
              WRITE    D01-REC
              ADD      1              TO   WK-RTCNT1
         WHEN 2
              MOVE     SPACE          TO   D02-REC
              INITIALIZE                   D02-REC
              MOVE     DEN-REC        TO   D02-REC
              MOVE     WK-TANABAN     TO   D02-F49
              WRITE    D02-REC
              ADD      1              TO   WK-RTCNT2
         WHEN 3
              MOVE     SPACE          TO   D03-REC
              INITIALIZE                   D03-REC
              MOVE     DEN-REC        TO   D03-REC
              MOVE     WK-TANABAN     TO   D03-F49
              WRITE    D03-REC
              ADD      1              TO   WK-RTCNT3
         WHEN 4
              MOVE     SPACE          TO   D04-REC
              INITIALIZE                   D04-REC
              MOVE     DEN-REC        TO   D04-REC
              MOVE     WK-TANABAN     TO   D04-F49
              WRITE    D04-REC
              ADD      1              TO   WK-RTCNT4
         WHEN 5
              MOVE     SPACE          TO   D05-REC
              INITIALIZE                   D05-REC
              MOVE     DEN-REC        TO   D05-REC
              MOVE     WK-TANABAN     TO   D05-F49
              WRITE    D05-REC
              ADD      1              TO   WK-RTCNT5
         WHEN 6
              MOVE     SPACE          TO   D06-REC
              INITIALIZE                   D06-REC
              MOVE     DEN-REC        TO   D06-REC
              MOVE     WK-TANABAN     TO   D06-F49
              WRITE    D06-REC
              ADD      1              TO   WK-RTCNT6
         WHEN 7
              MOVE     SPACE          TO   D07-REC
              INITIALIZE                   D07-REC
              MOVE     DEN-REC        TO   D07-REC
              MOVE     WK-TANABAN     TO   D07-F49
              WRITE    D07-REC
              ADD      1              TO   WK-RTCNT7
         WHEN 8
              MOVE     SPACE          TO   D08-REC
              INITIALIZE                   D08-REC
              MOVE     DEN-REC        TO   D08-REC
              MOVE     WK-TANABAN     TO   D08-F49
              WRITE    D08-REC
              ADD      1              TO   WK-RTCNT9
         WHEN 9
              MOVE     SPACE          TO   D09-REC
              INITIALIZE                   D09-REC
              MOVE     DEN-REC        TO   D09-REC
              MOVE     WK-TANABAN     TO   D09-F49
              WRITE    D09-REC
              ADD      1              TO   WK-RTCNT9
         WHEN 10
              MOVE     SPACE          TO   D10-REC
              INITIALIZE                   D10-REC
              MOVE     DEN-REC        TO   D10-REC
              MOVE     WK-TANABAN     TO   D10-F49
              WRITE    D10-REC
              ADD      1              TO   WK-RTCNT10
         WHEN 11
              MOVE     SPACE          TO   D11-REC
              INITIALIZE                   D11-REC
              MOVE     DEN-REC        TO   D11-REC
              MOVE     WK-TANABAN     TO   D11-F49
              WRITE    D11-REC
              ADD      1              TO   WK-RTCNT11
         WHEN 12
              MOVE     SPACE          TO   D12-REC
              INITIALIZE                   D12-REC
              MOVE     DEN-REC        TO   D12-REC
              MOVE     WK-TANABAN     TO   D12-F49
              WRITE    D12-REC
              ADD      1              TO   WK-RTCNT12
         WHEN 13
              MOVE     SPACE          TO   D13-REC
              INITIALIZE                   D13-REC
              MOVE     DEN-REC        TO   D13-REC
              MOVE     WK-TANABAN     TO   D13-F49
              WRITE    D13-REC
              ADD      1              TO   WK-RTCNT13
         WHEN 14
              MOVE     SPACE          TO   D14-REC
              INITIALIZE                   D14-REC
              MOVE     DEN-REC        TO   D14-REC
              MOVE     WK-TANABAN     TO   D14-F49
              WRITE    D14-REC
              ADD      1              TO   WK-RTCNT14
         WHEN 15
              MOVE     SPACE          TO   D15-REC
              INITIALIZE                   D15-REC
              MOVE     DEN-REC        TO   D15-REC
              MOVE     WK-TANABAN     TO   D15-F49
              WRITE    D15-REC
              ADD      1              TO   WK-RTCNT15
         WHEN 16
              MOVE     SPACE          TO   D16-REC
              INITIALIZE                   D16-REC
              MOVE     DEN-REC        TO   D16-REC
              MOVE     WK-TANABAN     TO   D16-F49
              WRITE    D16-REC
              ADD      1              TO   WK-RTCNT16
         WHEN 17
              MOVE     SPACE          TO   D17-REC
              INITIALIZE                   D17-REC
              MOVE     DEN-REC        TO   D17-REC
              MOVE     WK-TANABAN     TO   D17-F49
              WRITE    D17-REC
              ADD      1              TO   WK-RTCNT17
         WHEN 18
              MOVE     SPACE          TO   D18-REC
              INITIALIZE                   D18-REC
              MOVE     DEN-REC        TO   D18-REC
              MOVE     WK-TANABAN     TO   D18-F49
              WRITE    D18-REC
              ADD      1              TO   WK-RTCNT18
         WHEN 19
              MOVE     SPACE          TO   D19-REC
              INITIALIZE                   D19-REC
              MOVE     DEN-REC        TO   D19-REC
              MOVE     WK-TANABAN     TO   D19-F49
              WRITE    D19-REC
              ADD      1              TO   WK-RTCNT19
         WHEN 20
              MOVE     SPACE          TO   D20-REC
              INITIALIZE                   D20-REC
              MOVE     DEN-REC        TO   D20-REC
              MOVE     WK-TANABAN     TO   D20-F49
              WRITE    D20-REC
              ADD      1              TO   WK-RTCNT20
     END-EVALUATE.
*
 MAIN-010.
*
     READ     SHTDENLA
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                   ADD  1    TO   RD-CNT
     END-READ.
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
     CLOSE     SHTDENLA  JYOKENF  SHOTBL1.
     CLOSE     JHSDEN01.
     CLOSE     JHSDEN02.
     CLOSE     JHSDEN03.
     CLOSE     JHSDEN04.
     CLOSE     JHSDEN05.
     CLOSE     JHSDEN06.
     CLOSE     JHSDEN07.
     CLOSE     JHSDEN08.
     CLOSE     JHSDEN09.
     CLOSE     JHSDEN10.
     CLOSE     JHSDEN11.
     CLOSE     JHSDEN12.
     CLOSE     JHSDEN13.
     CLOSE     JHSDEN14.
     CLOSE     JHSDEN15.
     CLOSE     JHSDEN16.
     CLOSE     JHSDEN17.
     CLOSE     JHSDEN18.
     CLOSE     JHSDEN19.
     CLOSE     JHSDEN20.
     CLOSE     PRINTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　条件ファイル読込み　　　　　　　　　　　　　　*
****************************************************************
 JYOKENF-READ-SEC      SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
*****DISPLAY "DEN-F48 = " DEN-F48 UPON CONS.
     MOVE      "20"      TO      JYO-F01.
     MOVE      DEN-F48   TO      JYO-F02.
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
*********DISPLAY "RTCD = " WK-RTCDT(IX) " IX = " IX UPON CONS
         IF    WK-FLCDT(IX)   NOT =     ZERO
               WRITE     PRINT-REC FROM MEISAI   AFTER 1
               WRITE     PRINT-REC FROM SEN2     AFTER 1
         END-IF
     END-PERFORM.
 LISTWT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　商品変換テーブル読み込み　　　　　　　　　　　*
****************************************************************
 HSHOTBL-READ-SEC      SECTION.
*
     MOVE "HSHOTBL-REDA-SEC" TO      S-NAME.
*  商品変換テーブル検索
     MOVE      DEN-F01     TO        TBL-F01.
     MOVE      DEN-F25     TO        TBL-F02.
     READ      SHOTBL1
       INVALID
       MOVE    DEN-F49     TO        WK-TANABAN
       NOT  INVALID
       MOVE    TBL-F08     TO        WK-TANABAN
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
