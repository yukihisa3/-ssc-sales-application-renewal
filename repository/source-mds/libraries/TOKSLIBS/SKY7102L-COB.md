# SKY7102L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SKY7102L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　送り状印刷　　　　　　　　　　　  *
*    作成日／作成者　　　：　2010/11/19   /ABE                 *
*                            佐川急便新伝票対応                *
*    更新日／更新者　　　：　2018/06/04    NAV T.TAKAHASHI     *
*                            新組織対応　　　　　　　　　　　　*
*    再利用ＰＧ　　　　　：  SKY7102L.SKTSLIBS                 *
*    処理概要　　　　　　：　送り状Ｆを読み，画面より入力された*
*                            処理区分を判定し，送り状を印刷する*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKY7102L.
*AUTHER.                HAGIWARA.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITU.
 OBJECT-COMPUTER.       FUJITU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*-----<<  送り状ファイル  >>-----*
     SELECT   KOKURIF   ASSIGN    TO        DA-01-S-KOKURIF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   OKU-STS.
*-----<<  画面ファイル  >>-----*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        FILE      STATUS    IS   DSP-STATUS.
*-----<<  プリントファイル  >>-----*
     SELECT   PRTF      ASSIGN      TO      GS-PRTF
                        ACCESS MODE         IS  SEQUENTIAL
                        ORGANIZATION        IS  SEQUENTIAL
                        FORMAT              IS  PRT-FMT
                        GROUP               IS  PRT-GRP
                        PROCESSING  MODE    IS  PRT-PRO
                        UNIT CONTROL        IS  PRT-CON
                        DESTINATION         IS  "PRT"
                        FILE STATUS         IS  PRT-ST
                        DESTINATION-1       IS  PRT-DES.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*-----<<  送り状ファイル  >>-----*
 FD  KOKURIF.
     COPY     KOKURIF   OF   XFDLIB  JOINING   OKU  PREFIX.
*-----<<  画面ファイル  >>-----*
 FD  DSPF.
     COPY   FKY71011    OF        XMDLIB.
*-----<<  プリントファイル  >>-----*
 FD  PRTF.
 01  PRT-AREA                PIC  X(3000).
****************************************************************
*   ＷＯＲＫＩＮＧ－ＳＴＯＲＡＧＥ　　　ＳＥＣＴＩＯＮ
****************************************************************
 WORKING-STORAGE        SECTION.
****  プリントファイル  ****
 COPY   FKY71021         OF        XMDLIB.
 COPY   FKY71022         OF        XMDLIB.
 COPY   FKY11013         OF        XMDLIB.
 COPY   FKY11014         OF        XMDLIB.
 COPY   FKY11015         OF        XMDLIB.
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  プリンタ制御項目  ****
 01  PRT-CNTL.
     03  PRT-FMT             PIC  X(08).
     03  PRT-DES             PIC  X(08).
     03  PRT-GRP             PIC  X(08).
     03  PRT-PRO             PIC  X(02).
     03  PRT-ST              PIC  X(02).
     03  PRT-CON.
         05  PRT-CON1        PIC  X(04).
         05  PRT-CON2        PIC  9(02)     VALUE     ZERO.
****  ステイタス情報  ***
 01  STATUS-AREA.
     03  OKU-STS             PIC  X(02).
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  END-FLG1                PIC  X(03)  VALUE  SPACE.
 01  SYORI-FLG               PIC  9(01)  VALUE  ZERO.
 01  CHK-FLG                 PIC  9(01)  VALUE  ZERO.
 01  IX                      PIC  9(03).
 01  WORK-AREA.
     03  WK-SYURUI           PIC  9(01)  VALUE  ZERO.
****  カウント  エリア        ****
 01  CNT-AREA                VALUE    ZERO.
     03 IN-CNT1              PIC  9(07).
     03 IN-CNT2              PIC  9(07).
     03 PRT-CNT1             PIC  9(07).
     03 PRT-CNT2             PIC  9(07).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
****  （株）サカタのタネ殿エリア   ****
 01  WK-AREA.
     03 WK-PRT1              PIC  X(08)   VALUE  "224-0041".
     03 WK-PRT2              PIC  N(18)   VALUE
             NC"神奈川県横浜市都筑区仲町台２－７－１".
     03 WK-PRT3              PIC  N(12)   VALUE
             NC"_　サカタのタネ　　　　".
*************NC"_　サカタのタネ　特販部".
     03 WK-PRT4              PIC  X(12)   VALUE  "045-945-8816".
     03 WK-PRT5              PIC  N(12)   VALUE
             NC"（株）サカタのタネ　　　".
     03 WK-PRT6              PIC  N(12)   VALUE
*#2018/06/01 NAV ST
*************NC"　ホームガーデン部　　　".
             NC"　営業第２部　　　　　　".
*#2018/06/01 NAV ED
****  郵便番号編集
 01  YUBIN.
     03  YUBIN1              PIC  X(03)   VALUE  SPACE.
     03  HAIFUN              PIC  X(01)   VALUE  "-".
     03  YUBIN2              PIC  X(04)   VALUE  SPACE.
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     03  PMSG01            PIC N(20) VALUE  NC"_取消　_終了".
     03  PMSG02            PIC N(20) VALUE
                           NC"_取消　_終了　_項目戻し".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SKY7102L".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージ  ***
 01  ERR-TAB.
     03  MSG1                PIC  N(28)  VALUE
            NC"無効ＰＦキーです。".
     03  MSG2                PIC  N(28)  VALUE
            NC"１，２以外はエラーです。".
     03  MSG3                PIC  N(28)  VALUE
            NC"１，２，３，４以外はエラーです。".
     03  MSG4                PIC  N(28)  VALUE
            NC"処理区分が未入力です。".
     03  MSG5                PIC  N(28)  VALUE
            NC"伝票種類が未入力です。".
     03  MSG6                PIC  N(28)  VALUE
            NC"Ｙで入力して下さい".
     03  MSG7                PIC  N(28)  VALUE
            NC"用紙を変えて下さい。".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DSPF.
     MOVE     "DSPF    "       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
***
 FILEERR-SEC2        SECTION.
     USE AFTER    EXCEPTION
                  PROCEDURE    PRTF.
     MOVE   "PRTF"             TO   ERR-FL-ID.
     MOVE    PRT-ST            TO   ERR-STCD.
     DISPLAY   MSG-ABEND1      UPON   CONS.
     DISPLAY   MSG-ABEND2      UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KOKURIF.
     MOVE     "KOKURIF"        TO   ERR-FL-ID.
     MOVE      OKU-STS         TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
************************************************************
*      0.0      メインモジュール                           *
************************************************************
 SKY7102L-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 SKY7102L-END.
     EXIT.
************************************************************
*      1.0      初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
* ファイルＯＰＥＮ
     OPEN     INPUT     KOKURIF.
     OPEN     I-O       DSPF.
     OPEN     OUTPUT    PRTF.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     MOVE    1               TO   SYORI-FLG.
 INIT-END.
     EXIT.
************************************************************
*      2.0       メイン処理                                *
************************************************************
 MAIN-SEC          SECTION.
     EVALUATE      SYORI-FLG
         WHEN      1    PERFORM   INIT-DSP-SEC
         WHEN      2    PERFORM   HEAD-SEC
         WHEN      3    PERFORM   KAKNIN-SEC
         WHEN      4    PERFORM   LST1-SEC
         WHEN      5    PERFORM   CL-OP-SEC
         WHEN      6
               IF   OKU-F01  =  1
                    PERFORM   LST3-SEC
               ELSE
                    PERFORM   LST2-SEC
               END-IF
*******  WHEN      6    PERFORM   LST2-SEC
         WHEN      7    PERFORM   TEST-SEC
     END-EVALUATE.
 MAIN-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
* ファイルＣＬＯＳＥ
     CLOSE    KOKURIF   DSPF  PRTF.
     DISPLAY "(SKY7102L)ﾆｭｳﾘｮｸ ｹﾝｽｳ(ｵﾔ) = " IN-CNT1  UPON  CONS.
     DISPLAY "(SKY7102L)ﾆｭｳﾘｮｸ ｹﾝｽｳ(ｺ)  = " IN-CNT2  UPON  CONS.
     DISPLAY "(SKY7102L)ﾌﾟﾘﾝﾄ  ｹﾝｽｳ(ｵﾔ) = " PRT-CNT1 UPON  CONS.
     DISPLAY "(SKY7102L)ﾌﾟﾘﾝﾄ  ｹﾝｽｳ(ｺ)  = " PRT-CNT2 UPON  CONS.
 END-END.
     EXIT.
*----------------------------------------------------------*
*      2.1       初期画面処理                              *
*----------------------------------------------------------*
 INIT-DSP-SEC        SECTION.
     MOVE     SPACE          TO   FKY71011.
     MOVE    "FKY71011"      TO   DSP-FORMAT.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
*
     MOVE   "M"     TO  EDIT-OPTION  OF  R00001
                        EDIT-OPTION  OF  R00002.
     MOVE  " "      TO  EDIT-CURSOR  OF  R00001
                        EDIT-CURSOR  OF  R00002.
     MOVE     2              TO   SYORI-FLG.
     MOVE     0              TO   CHK-FLG.
 INIT-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2       ＨＥＡＤ処理           　　               *
*----------------------------------------------------------*
 HEAD-SEC            SECTION.
     MOVE     PMSG01         TO   PFGID.
     PERFORM         DSP-WRITE-SEC.
*
     MOVE    "HEAD"          TO   DSP-GROUP.
     PERFORM         DSP-READ-SEC.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE   1           TO   SYORI-FLG
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "E000"
                        PERFORM   HEAD-CHK-SEC
         WHEN    OTHER
                        MOVE   MSG1      TO   ERRMSG
     END-EVALUATE.
 HEAD1-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1     ＨＥＡＤ部の入力チェック                  *
*----------------------------------------------------------*
 HEAD-CHK-SEC             SECTION.
*処理区分未入力チェック
     IF  ( R00001  NOT  NUMERIC   )
         MOVE   MSG4      TO   ERRMSG
         MOVE   "R"     TO   EDIT-OPTION  OF  R00001
         MOVE   "C"     TO   EDIT-CURSOR  OF  R00001
         GO     TO      HEAD-CHK-EXIT
     END-IF.
*処理区分チェック
     IF  (  R00001 =  1  OR  2  )
         CONTINUE
     ELSE
         MOVE   MSG2    TO   ERRMSG
         MOVE   "R"     TO   EDIT-OPTION  OF  R00001
         MOVE   "C"     TO   EDIT-CURSOR  OF  R00001
         GO     TO      HEAD-CHK-EXIT
     END-IF.
     MOVE   "M"     TO   EDIT-OPTION  OF  R00001.
     MOVE   " "     TO   EDIT-CURSOR  OF  R00001.
*伝票種類未入力チェック
     IF  ( R00002  NOT  NUMERIC   )
         MOVE   MSG5      TO   ERRMSG
         MOVE   "R"     TO   EDIT-OPTION  OF  R00002
         MOVE   "C"     TO   EDIT-CURSOR  OF  R00002
         GO     TO      HEAD-CHK-EXIT
     END-IF.
*伝票種類チェック
     IF  (  R00002 =  1  OR  2  OR  3  OR  4)
         EVALUATE    R00002
             WHEN    1
                     MOVE    NC"佐川急便"     TO  W00002
             WHEN    2
                     MOVE    NC"ヤマト運輸"   TO  W00002
             WHEN    3
                     MOVE    NC"日本通運"     TO  W00002
             WHEN    4
                     MOVE    NC"小口　　"     TO  W00002
         END-EVALUATE
     ELSE
         MOVE   SPACE   TO   W00002
         MOVE   MSG3    TO   ERRMSG
         MOVE   "R"     TO   EDIT-OPTION  OF  R00002
         MOVE   "C"     TO   EDIT-CURSOR  OF  R00002
         GO     TO      HEAD-CHK-EXIT
     END-IF.
     MOVE   "M"     TO   EDIT-OPTION  OF  R00002.
     MOVE   " "     TO   EDIT-CURSOR  OF  R00002.
*
     IF  (R00001    =   2)  AND (R00002    =   4)
          MOVE    5             TO   SYORI-FLG
     ELSE
          MOVE    3             TO   SYORI-FLG
     END-IF.
***  MOVE     3              TO   SYORI-FLG.
     MOVE   SPACE            TO   ERRMSG.
 HEAD-CHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.3       確認入力処理                              *
*----------------------------------------------------------*
 KAKNIN-SEC       SECTION.
     MOVE     "Y"            TO   R00003.
     MOVE     PMSG02         TO   PFGID.
     PERFORM       DSP-WRITE-SEC.
*
     MOVE    "KAKNIN"        TO    DSP-GROUP.
     PERFORM       DSP-READ-SEC.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        MOVE    1      TO   SYORI-FLG
         WHEN   "F005"
                        MOVE  "END"    TO   END-FLG
         WHEN   "F006"
                        MOVE   "M"  TO   EDIT-OPTION  OF  R00002
                        MOVE   " "  TO   EDIT-CURSOR  OF  R00002
                        MOVE    2      TO   SYORI-FLG
                        MOVE    SPACE  TO   R00003
         WHEN   "E000"
                        IF  R00003  NOT  =  "Y"
                            MOVE    MSG6       TO  ERRMSG
                        ELSE
                            IF  R00001  =   2
                                PERFORM  OKU-READ-SEC
                                IF  END-FLG1  =  "END"
                                    MOVE   1    TO   SYORI-FLG
                                    PERFORM   CL-OP-SEC2
                                ELSE
                                    IF  R00002  =  OKU-F01
                                        MOVE  4      TO  SYORI-FLG
                                    END-IF
                                END-IF
                            ELSE
                                PERFORM  OKU-READ-SEC2
                                MOVE   7        TO   SYORI-FLG
                            END-IF
                        END-IF
         WHEN    OTHER
                        MOVE    MSG1   TO   ERRMSG
     END-EVALUATE.
 KAKNIN-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4       ＬＩＳＴ処理  （親）                      *
*----------------------------------------------------------*
 LST1-SEC         SECTION.
* 伝票種類判定
     EVALUATE   R00002
         WHEN   1     PERFORM  PRT1-SEC
         WHEN   2     PERFORM  PRT2-SEC
         WHEN   3     PERFORM  PRT3-SEC
     END-EVALUATE.
* 送り状ファイルＲＥＡＤ
     PERFORM   OKU-READ-SEC.
 LST1-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.1     ＬＩＳＴ処理  ( 佐川急便）                *
*----------------------------------------------------------*
 PRT1-SEC         SECTION.
     MOVE     SPACE            TO   FKY71021.
* ファイルからＰＲＴへ
***  郵便番号
     MOVE  OKU-F151            TO   YUBIN1.
     MOVE  OKU-F152            TO   YUBIN2.
*****MOVE  YUBIN               TO   W10001.
     MOVE  YUBIN(1:1)          TO   YBN1.
     MOVE  YUBIN(2:1)          TO   YBN2.
     MOVE  YUBIN(3:1)          TO   YBN3.
     MOVE  YUBIN(5:1)          TO   YBN4.
     MOVE  YUBIN(6:1)          TO   YBN5.
     MOVE  YUBIN(7:1)          TO   YBN6.
     MOVE  YUBIN(8:1)          TO   YBN7.
**
     MOVE  OKU-F11             TO   W10002.
     MOVE  OKU-F07             TO   W10003.
     MOVE  OKU-F08             TO   W10004.
     MOVE  OKU-F03             TO   W10005.
     MOVE  OKU-F05             TO   W10006.
     MOVE  OKU-F06             TO   W10007.
     MOVE  OKU-F09             TO   W10008.
     MOVE  OKU-F10             TO   W10009.
* 発送元印刷チェック
     IF   OKU-F14  =  1
          MOVE  WK-PRT1             TO   W11001
          MOVE  WK-PRT2(1:9)        TO   W11002
          MOVE  WK-PRT2(10:9)       TO   W11003
          MOVE  WK-PRT3             TO   W11004
          MOVE  WK-PRT4             TO   W11005
          MOVE  WK-PRT6             TO   W11006
     END-IF.
* 送り状印刷
     MOVE     SPACE            TO   PRT-PRO.
     MOVE     "FKY71021"       TO   PRT-FMT.
     MOVE     SPACE            TO   PRT-CON1.
     MOVE     "GRP101"         TO   PRT-GRP.
     PERFORM   VARYING     IX     FROM   1   BY   1  UNTIL
                                      IX   >   OKU-F12
               WRITE    PRT-AREA       FROM    FKY71021
               ADD      1                TO    PRT-CNT1
*              CLOSE    PRTF
*              OPEN    OUTPUT   PRTF
     END-PERFORM.
 PRT1-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.2     ＬＩＳＴ処理  ( ヤマト運輸）              *
*----------------------------------------------------------*
 PRT2-SEC         SECTION.
     MOVE     SPACE            TO   FKY11013.
* ファイルからＰＲＴへ
***  郵便番号
     MOVE  OKU-F151            TO   YUBIN1.
     MOVE  OKU-F152            TO   YUBIN2.
     MOVE  YUBIN               TO   W20001.
***
     MOVE  OKU-F07             TO   W20003.
     MOVE  OKU-F08             TO   W20004.
     MOVE  OKU-F03             TO   W20005.
     MOVE  OKU-F05             TO   W20006.
     MOVE  OKU-F06             TO   W20007.
     MOVE  OKU-F09             TO   W20008.
     MOVE  OKU-F10             TO   W20009.
* 発送元印刷チェック
     IF   OKU-F14  =  1
          MOVE  WK-PRT1             TO   W22001
          MOVE  WK-PRT2(1:9)        TO   W22002
          MOVE  WK-PRT2(10:9)       TO   W22003
**********MOVE  WK-PRT3             TO   W22004
          MOVE  WK-PRT5             TO   W22004
*20070112*MOVE  WK-PRT6             TO   W22014
          MOVE  WK-PRT4             TO   W22005
     END-IF.
* 送り状印刷
     MOVE     SPACE            TO   PRT-PRO.
     MOVE     "FKY11013"       TO   PRT-FMT.
     MOVE     SPACE            TO   PRT-CON1.
     MOVE     "GRP201"         TO   PRT-GRP.
     PERFORM   VARYING     IX     FROM   1   BY   1  UNTIL
                                      IX   >   OKU-F12
               WRITE    PRT-AREA       FROM    FKY11013
               ADD      1                TO    PRT-CNT1
*              CLOSE    PRTF
*              OPEN    OUTPUT   PRTF
     END-PERFORM.
 PRT2-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     ＬＩＳＴ処理  ( 日本通運）                *
*----------------------------------------------------------*
 PRT3-SEC         SECTION.
     MOVE     SPACE            TO   FKY11014.
* ファイルからＰＲＴへ
***  郵便番号
     MOVE  OKU-F151            TO   YUBIN1.
     MOVE  OKU-F152            TO   YUBIN2.
     MOVE  YUBIN               TO   W30001.
***
     MOVE  OKU-F11             TO   W30002.
     MOVE  OKU-F07             TO   W30003.
     MOVE  OKU-F08             TO   W30004.
     MOVE  OKU-F03             TO   W30005.
     MOVE  OKU-F05             TO   W30006.
     MOVE  OKU-F06             TO   W30007.
     MOVE  OKU-F09             TO   W30008.
     MOVE  OKU-F10             TO   W30009.
* 発送元印刷チェック
     IF   OKU-F14  =  1
          MOVE  WK-PRT1             TO   W33001
          MOVE  WK-PRT2(1:9)        TO   W33002
          MOVE  WK-PRT2(10:9)       TO   W33003
          MOVE  WK-PRT3             TO   W33004
          MOVE  WK-PRT4             TO   W33005
     END-IF.
* 送り状印刷
     MOVE     SPACE            TO   PRT-PRO.
     MOVE     "FKY11014"       TO   PRT-FMT.
     MOVE     SPACE            TO   PRT-CON1.
     MOVE     "GRP301"         TO   PRT-GRP.
     PERFORM   VARYING     IX     FROM   1   BY   1  UNTIL
                                      IX   >   OKU-F12
               WRITE    PRT-AREA       FROM    FKY11014
               ADD      1                TO    PRT-CNT1
*              CLOSE    PRTF
*              OPEN    OUTPUT   PRTF
     END-PERFORM.
 PRT3-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.5       ファイルＣＬＯＳＥ,ＯＰＥＮ処理           *
*----------------------------------------------------------*
 CL-OP-SEC        SECTION.
* ファイルＣＬＯＳＥ
     CLOSE    KOKURIF.
* ファイルＯＰＥＮ
     OPEN     INPUT     KOKURIF.
     CLOSE              PRTF.
     OPEN     OUTPUT    PRTF.
* 子リスト存在チェック
**   IF   CHK-FLG    =    ZERO
**        PERFORM   CL-OP-SEC2
**        MOVE    1             TO   SYORI-FLG
**        GO    TO    CL-OP-EXIT
**   END-IF.
* メッセージ出力
     MOVE     "Y"            TO   R00003.
     MOVE     PMSG02         TO   PFGID.
     MOVE     MSG7           TO   W00002.
     PERFORM         DSP-WRITE-SEC.
*
     MOVE    "KAKNIN"        TO    DSP-GROUP.
     PERFORM       DSP-READ-SEC.
* アテンション判定
     EVALUATE   DSP-FUNC
         WHEN   "F004"
                        PERFORM   CL-OP-SEC2
                        MOVE    1      TO   SYORI-FLG
         WHEN   "F005"
                        MOVE  "END"    TO   END-FLG
         WHEN   "F006"
                        PERFORM   CL-OP-SEC2
                        MOVE    2      TO   SYORI-FLG
                        MOVE    SPACE  TO   R00003
         WHEN    "E000"
                   IF  R00003  NOT  =  "Y"
                       MOVE    MSG6       TO  ERRMSG
                   ELSE
                       PERFORM   OKU-READ-SEC2
                       IF     SYORI-FLG  NOT = 1
                           MOVE    6             TO   SYORI-FLG
                       END-IF
                   END-IF
         WHEN    OTHER
                   MOVE    MSG1   TO   ERRMSG
     END-EVALUATE.
 CL-OP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.6       ＬＩＳＴ処理  （子）佐川急便以外          *
*----------------------------------------------------------*
 LST2-SEC         SECTION.
     MOVE     SPACE            TO   FKY11015.
* ファイルからＰＲＴへ
***  郵便番号
     MOVE  OKU-F151            TO   YUBIN1.
     MOVE  OKU-F152            TO   YUBIN2.
     MOVE  YUBIN               TO   W40001.
***
     MOVE  OKU-F11             TO   W40002.
     MOVE  OKU-F07             TO   W40003.
     MOVE  OKU-F08             TO   W40004.
     MOVE  OKU-F03             TO   W40005.
     MOVE  OKU-F05             TO   W40006.
     MOVE  OKU-F06             TO   W40007.
     MOVE  OKU-F09             TO   W40008.
**** MOVE  OKU-F10(1:8)        TO   W40009.
* 伝票種類判定
     EVALUATE    OKU-F01
         WHEN    1
                 MOVE    NC"佐川急便"     TO  W40009
         WHEN    2
                 MOVE    NC"ヤマト運輸"   TO  W40009
         WHEN    3
                 MOVE    NC"日本通運"     TO  W40009
         WHEN    OTHER
                 MOVE    SPACE            TO  W40009
     END-EVALUATE.
* 送り状印刷
     MOVE     SPACE            TO   PRT-PRO.
     MOVE     "FKY11015"       TO   PRT-FMT.
     MOVE     SPACE            TO   PRT-CON1.
     MOVE     "GRP401"         TO   PRT-GRP.
     PERFORM   VARYING     IX     FROM   1   BY   1  UNTIL
                                      IX   >   OKU-F13
               WRITE    PRT-AREA       FROM    FKY11015
               ADD      1                TO    PRT-CNT2
*              CLOSE    PRTF
*              OPEN    OUTPUT   PRTF
     END-PERFORM.
* 送り状ファイルＲＥＡＤ
     PERFORM   OKU-READ-SEC2.
 LST2-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7       ＬＩＳＴ処理  （子）佐川急便              *
*----------------------------------------------------------*
 LST3-SEC         SECTION.
     MOVE     SPACE            TO   FKY71022.
* ファイルからＰＲＴへ
***  郵便番号
     MOVE  OKU-F151            TO   YUBIN1.
     MOVE  OKU-F152            TO   YUBIN2.
     MOVE  YUBIN               TO   W50001.
***
     MOVE  OKU-F11             TO   W50002.
     MOVE  OKU-F07             TO   W50003.
     MOVE  OKU-F08             TO   W50004.
     MOVE  OKU-F03             TO   W50005.
     MOVE  OKU-F05             TO   W50006.
     MOVE  OKU-F06             TO   W50007.
     MOVE  OKU-F09             TO   W50008.
     MOVE  OKU-F10             TO   W50009.
* 発送元印刷チェック
     IF   OKU-F14  =  1
          MOVE  WK-PRT1             TO   W51001
          MOVE  WK-PRT2(1:9)        TO   W51002
          MOVE  WK-PRT2(10:9)       TO   W51003
          MOVE  WK-PRT3             TO   W51004
          MOVE  WK-PRT6             TO   W51005
          MOVE  WK-PRT4             TO   W51006
     END-IF.
* 送り状印刷
     MOVE     SPACE            TO   PRT-PRO.
     MOVE     "FKY71022"       TO   PRT-FMT.
     MOVE     SPACE            TO   PRT-CON1.
     MOVE     "GRP501"         TO   PRT-GRP.
     PERFORM   VARYING     IX     FROM   1   BY   1  UNTIL
                                      IX   >   OKU-F13
               WRITE    PRT-AREA       FROM    FKY71022
               ADD      1                TO    PRT-CNT2
*              CLOSE    PRTF
*              OPEN    OUTPUT   PRTF
     END-PERFORM.
     PERFORM   OKU-READ-SEC2.
 PRT3-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.8       ＬＩＳＴ処理  ( テストプリント）          *
*----------------------------------------------------------*
 TEST-SEC         SECTION.
* 伝票種類判定, 送り状印刷（親テスト）
     EVALUATE   R00002
         WHEN   1
                MOVE  SPACE               TO   FKY71021
****************MOVE  ALL "*"             TO   W10001  W10008
                MOVE  ALL "*"             TO   W10008
                                               MAS101  MAS102
                                               YBN1  YBN2  YBN3
                                               YBN4  YBN5  YBN6
                                               YBN7
                MOVE  ALL NC"＊"          TO   W10003  W10004
                                               W10006  W10007
                                               W10009
                MOVE     SPACE            TO   PRT-PRO
                MOVE     "FKY71021"       TO   PRT-FMT
                MOVE     SPACE            TO   PRT-CON1
                MOVE     "GRP101"         TO   PRT-GRP
                WRITE    PRT-AREA       FROM    FKY71021
         WHEN   2
                MOVE  SPACE               TO   FKY11013
                MOVE  ALL "*"             TO   W20001  W20008
                                               MAS202
                MOVE  ALL NC"＊"          TO   W20003  W20004
                                               W20006  W20007
                                               W20009
                MOVE     SPACE            TO   PRT-PRO
                MOVE     "FKY11013"       TO   PRT-FMT
                MOVE     SPACE            TO   PRT-CON1
                MOVE     "GRP201"         TO   PRT-GRP
                WRITE    PRT-AREA       FROM    FKY11013
         WHEN   3
                MOVE  SPACE               TO   FKY11014
                MOVE  ALL "*"             TO   W30001  W30008
                                               MAS301  MAS302
                MOVE  ALL NC"＊"          TO   W30003  W30004
                                               W30006  W30007
                                               W30009  W30010
                MOVE     SPACE            TO   PRT-PRO
                MOVE     "FKY11014"       TO   PRT-FMT
                MOVE     SPACE            TO   PRT-CON1
                MOVE     "GRP301"         TO   PRT-GRP
                WRITE    PRT-AREA       FROM    FKY11014
         WHEN   4
                DISPLAY " OKU-F01 = " OKU-F01 UPON CONS
                IF    OKU-F01 =   1
                    MOVE  SPACE           TO   FKY71022
                    INITIALIZE                 FKY71022
                    MOVE  ALL "9"         TO   MAS501  MAS502
                    MOVE  ALL "*"         TO   W50001  W50008
                    MOVE  ALL NC"＊"      TO   W50003  W50004
                                               W50006  W50007
                                               W50009
                    MOVE     SPACE        TO   PRT-PRO
                    MOVE     "FKY71022"   TO   PRT-FMT
                    MOVE     SPACE        TO   PRT-CON1
                    MOVE     "GRP501"     TO   PRT-GRP
                    WRITE    PRT-AREA   FROM    FKY71022
                ELSE
                    MOVE  SPACE           TO   FKY11015
                    MOVE  SPACE           TO   FKY11015
                    MOVE  ALL "*"         TO   W40001  W40008
                    MOVE  ALL NC"＊"      TO   W40003  W40004
                                               W40006  W40007
                                               W40009
                    MOVE     SPACE        TO   PRT-PRO
                    MOVE     "FKY11015"   TO   PRT-FMT
                    MOVE     SPACE        TO   PRT-CON1
                    MOVE     "GRP401"     TO   PRT-GRP
                    WRITE    PRT-AREA   FROM   FKY11015
                END-IF
     END-EVALUATE.
     MOVE     1                TO   SYORI-FLG.
     CLOSE    PRTF.
     OPEN     OUTPUT    PRTF.
 TEST-EXIT.
     EXIT.
*----------------------------------------------------------*
*                送り状ＦＲＥＡＤ処理                      *
*----------------------------------------------------------*
 OKU-READ-SEC           SECTION.
     READ    KOKURIF
             AT   END
                  MOVE   "END"    TO   END-FLG1
                  MOVE R00002     TO   WK-SYURUI
                  MOVE   5        TO   SYORI-FLG
       NOT   AT   END
                  IF   R00002  NOT   =   OKU-F01
                       GO    TO   OKU-READ-SEC
                  END-IF
                  ADD    1        TO   IN-CNT1
                  IF     OKU-F13   NOT   =   ZERO
                         MOVE    1       TO   CHK-FLG
                  END-IF
     END-READ.
 OKU-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*                送り状ＦＲＥＡＤ処理                      *
*----------------------------------------------------------*
 OKU-READ-SEC2          SECTION.
     READ    KOKURIF
             AT   END
                  MOVE   1        TO   SYORI-FLG
                  PERFORM   CL-OP-SEC2
       NOT   AT   END
                  IF   OKU-F13    =    ZERO
                       GO    TO   OKU-READ-SEC2
                  END-IF
                  ADD    1        TO   IN-CNT2
     END-READ.
 OKU-READ2-EXIT.
     EXIT.
*----------------------------------------------------------*
*                ファイルＣＬＯＳＥ,ＯＰＥＮ処理           *
*----------------------------------------------------------*
 CL-OP-SEC2       SECTION.
* ファイルＣＬＯＳＥ
     CLOSE    KOKURIF.
* ファイルＯＰＥＮ
     OPEN     INPUT     KOKURIF.
     CLOSE              PRTF.
     OPEN     OUTPUT    PRTF.
     MOVE     SPACE     TO     END-FLG1.
 CL-OP2-EXIT.
     EXIT.
*----------------------------------------------------------*
*                画面表示処理                              *
*----------------------------------------------------------*
 DSP-WRITE-SEC         SECTION.

     MOVE  NC"１：佐川　２：ヤマト　３：日通　４：小口"
                             TO   W00002.
     MOVE    "FKY71011"      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
*
     WRITE    FKY71011.
 DSP-WRITE-EXIT.
     EXIT.
*----------------------------------------------------------*
*                画面データの入力処理                      *
*----------------------------------------------------------*
 DSP-READ-SEC           SECTION.
     MOVE  "NE"    TO   DSP-PROC.
     READ   DSPF.
 DSP-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
