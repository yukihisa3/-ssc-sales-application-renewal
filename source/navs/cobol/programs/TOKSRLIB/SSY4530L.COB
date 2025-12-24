****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　流通ＢＭＳ（ハンズマン）　
*    業務名　　　　　　　：　返品　　　　
*    モジュール名　　　　：　流通ＢＭＳ返品リスト発行
*    作成日／作成者　　　：　2023/11/20 NAV TAKAHASHI
*    処理概要　　　　　　：　発行指示より受け取ったパラメータを
*                        ：　条件に、流通ＢＭＳ返品リスト発行を
*                        ：　する。
*    更新履歴　　　　　　：
*    更新日／更新者　　　：　
*
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SSY4530L.
*                     流用:SBM0172L（ハンズマン）
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2018/09/10.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
*    STATION     IS        STA
     YA          IS        NIHONGO
     YB          IS        YB
     YB-21       IS        YB-21
     YB-22       IS        YB-22
     CONSOLE     IS        CONS.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*流通ＢＭＳ受領書ワーク（ハンズマン）
     SELECT     BMSHE7W    ASSIGN    TO  DA-01-VI-BMSHE7W1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       HEP-F013
                                               HEP-F04
                                               HEP-F07
                                               HEP-F03
                                               HEP-F09
                           FILE      STATUS    HEP-ST.
*%* プリンター *%*
     SELECT     PRTF       ASSIGN    TO        LP-04-PRTF.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*流通ＢＭＳ返品伝票ワーク
 FD  BMSHE7W
     LABEL       RECORD    IS        STANDARD.
     COPY        BMSHE7W   OF        XFDLIB
     JOINING     HEP       AS        PREFIX.

*プリンター
 FD    PRTF      LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
 01  FILE-STATUS.
     03  PRT-ST              PIC  X(02).
     03  PRT-ST1             PIC  X(04).
     03  HEP-ST              PIC  X(02).
 01  WK-AREA.
     03  END-SW              PIC  9(01)  VALUE   ZERO.
     03  ERR-SW              PIC  9(01)  VALUE   ZERO.
     03  PAGE-CNT            PIC  9(05)  VALUE   ZERO.
     03  LINE-CNT            PIC  9(05)  VALUE   ZERO.
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME             PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS               PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y            PIC  9(02)  VALUE  ZERO.
         05  WK-M            PIC  9(02)  VALUE  ZERO.
         05  WK-D            PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE            PIC  9(08).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
*
 01  IN-DATA                 PIC  X(01).
 01  FILE-ERR.
     03  HEP-ERR             PIC  N(10) VALUE
         NC"返品伝票ワークエラー".
     03  PRT-ERR             PIC  N(10) VALUE
         NC"プリンターエラー".
*
 01  IX                      PIC  9(04) VALUE  0.
 01  READ-CNT                PIC  9(07) VALUE  0.
 01  IN-CNT                  PIC  9(07) VALUE  0.
 01  SET-FLG                 PIC  X(03) VALUE  SPACE.
 01  HENKAN-FLG              PIC  X(03) VALUE  SPACE.
 01  DEN-FLG                 PIC  9(01) VALUE  0.
 01  HIT-FLG                 PIC  9(01) VALUE  0.
*
 01  NEW-KEY.
     02  NEW-TENCD           PIC  9(08).
     02  NEW-DENNO           PIC  9(09).
*
 01  OLD-KEY.
     02  OLD-TENCD           PIC  9(08).
     02  OLD-DENNO           PIC  9(09).
*
 01  HEN-GNOX.
     02  HEN-GNO             PIC  9(01).
     02  FILLER              PIC  X(03).
*
 01  HEN-DENNOX.
     02  HEN-DENNO           PIC  9(06).
     02  FILLER              PIC  X(04).
*
*帳票出力定義エリア
****  見出し行１　　　　　　 ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SSY4530L".
     02  FILLER              PIC  X(38)  VALUE  SPACE.
     02  FILLER              PIC  N(10)
         CHARACTER  TYPE  IS  YB-22    VALUE
         NC"＜返品伝票リスト＞（".
     02  HITTL               PIC  N(03)
         CHARACTER  TYPE  IS  YB-22.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YB-22    VALUE
         NC"）".
     02  FILLER              PIC  X(24)  VALUE  SPACE.
     02  SYSYY               PIC  9999.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"年".
     02  SYSMM               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"月".
     02  SYSDD               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"日".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  LPAGE               PIC  ZZ9.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"頁".
****  見出し行２***
 01  MIDASI-2.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MID2-01             PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO.
*********NC"受信日：".
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"：".
     02  DATEFROM            PIC  9(08).
     02  FILLER              PIC  X(03)  VALUE  " - ".
     02  DATETO              PIC  9(08).
****  見出し行３***
 01  MIDASI-3.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"取引先：".
     02  TORINAM             PIC  N(16)
         CHARACTER  TYPE  IS  YB.
****  見出し行４***
 01  MIDASI-4.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"店舗情報".
*
****  見出し行５***
 01  MIDASI-5.
* 2023/11/20 UPD START
*    02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
* 2023/11/20 UPD END
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"処理種別".
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
* 2023/11/20 UPD START
*    02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
* 2023/11/20 UPD END
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"計上日".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(02)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"部門".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"伝票番号".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"行".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"商品情報".
*    02  FILLER              PIC  X(26)  VALUE  SPACE.
     02  FILLER              PIC  X(28)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"返品数".
* 2023/11/20 UPD START
*    02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
* 2023/11/20 UPD END
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"原単価".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"原価金額".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"売単価".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"売価金額".
* 2023/11/20 ADD START
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"譲渡年月".
     02  FILLER              PIC  X(03)  VALUE  "1,2".
* 2023/11/20 ADD END
*
****  線１　　　　　 ****
 01  HASEN-1.
     02  FILLER              PIC  X(136) VALUE  ALL "=".
****  線２　　　　　 ****
 01  HASEN-2.
*    02  FILLER              PIC  X(38) VALUE  SPACE.
     02  FILLER              PIC  X(39) VALUE  SPACE.
*    02  FILLER              PIC  X(98) VALUE  ALL "-".
     02  FILLER              PIC  X(97) VALUE  ALL "-".
****  線３　　　　　 ****
 01  HASEN-3.
     02  FILLER              PIC  X(136) VALUE  ALL "-".
*
****  明細行１　　　　　　　 ****
 01  MEISAI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TENCD               PIC  ZZZZ9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TENNM               PIC  X(15).
*
****  明細行２　　　　　　　 ****
 01  MEISAI-2.
* 2023/11/20 DEL START
*    02  FILLER              PIC  X(03)  VALUE  SPACE.
* 2023/11/20 DEL END
*****02  DENKUNM             PIC  N(01)
*****                        CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SYRKBN              PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  SYRKBNNM            PIC  N(02)
     02  SYRKBNNM            PIC  N(05)
                             CHARACTER  TYPE  IS  NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  KEIYMD              PIC  9(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  BUMON               PIC  X(04).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  DENNO               PIC  9(09).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  GNO                 PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  HINCD               PIC  X(13).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  HINKANA             PIC  X(20).
* 2023/11/20 UPD START
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  HENBKSU             PIC  -,---,--9.
     02  HENBKSU             PIC  ----,--9.
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  GENTANKA            PIC  -,---,--9.
     02  GENTANKA            PIC  ----,--9.
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
*    02  GENKINGK            PIC  ---,---,--9.
     02  GENKINGK            PIC  --,---,--9.
* 2023/11/20 UPD END
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  BAITANKA            PIC  ----,--9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  BAIKINGK            PIC  --,---,--9.
* 2023/11/20 UPD START
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  JYOTOYM1            PIC  X(06).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  JYOTOYM2            PIC  X(06).
* 2023/11/20 UPD END
*
****  伝票計　　　　　　　 ****
 01  GOKEI-1.
*    02  FILLER              PIC  X(58)  VALUE  SPACE.
*    02  FILLER              PIC  X(64)  VALUE  SPACE.
     02  FILLER              PIC  X(62)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
*        NC"＜伝票合計＞"      CHARACTER  TYPE  IS  YB.
         NC"＜伝票合計＞"      CHARACTER  TYPE  IS  NIHONGO.
*    02  FILLER              PIC  X(08)  VALUE  SPACE.
*    02  HENBKSU1            PIC  -,---,--9.
*    02  FILLER              PIC  X(11)  VALUE  SPACE.
*    02  GENKINGK1           PIC  ---,---,--9.
*    02  FILLER              PIC  X(11)  VALUE  SPACE.
*    02  BAIKINGK1           PIC  ---,---,--9.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  HENBKSU1            PIC  ----,--9.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  GENKINGK1           PIC  ----,---,--9.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  BAIKINGK1           PIC  ---,---,--9.
*
****  店舗計　　　　　　　 ****
 01  GOKEI-2.
*    02  FILLER              PIC  X(58)  VALUE  SPACE.
*    02  FILLER              PIC  X(64)  VALUE  SPACE.
     02  FILLER              PIC  X(62)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
*        NC"＜店舗合計＞"      CHARACTER  TYPE  IS  YB.
         NC"＜店舗合計＞"      CHARACTER  TYPE  IS  NIHONGO.
* 2023/11/20 UPD START
*    02  FILLER              PIC  X(08)  VALUE  SPACE.
*    02  HENBKSU2            PIC  -,---,--9.
*    02  FILLER              PIC  X(11)  VALUE  SPACE.
*    02  GENKINGK2           PIC  ---,---,--9.
*    02  FILLER              PIC  X(11)  VALUE  SPACE.
*    02  BAIKINGK2           PIC  ---,---,--9.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  HENBKSU2            PIC  ----,--9.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  GENKINGK2           PIC  ----,---,--9.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  BAIKINGK2           PIC  ---,---,--9.
* 2023/11/20 UPD END
*
****  総合計　　　　　　　 ****
 01  GOKEI-3.
*    02  FILLER              PIC  X(58)  VALUE  SPACE.
*    02  FILLER              PIC  X(64)  VALUE  SPACE.
     02  FILLER              PIC  X(62)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
*        NC"　＜総合計＞"      CHARACTER  TYPE  IS  YB.
         NC"　＜総合計＞"      CHARACTER  TYPE  IS  NIHONGO.
* 2023/11/20 UPD START
*    02  FILLER              PIC  X(08)  VALUE  SPACE.
*    02  HENBKSU3            PIC  -,---,--9.
*    02  FILLER              PIC  X(11)  VALUE  SPACE.
*    02  GENKINGK3           PIC  ---,---,--9.
*    02  FILLER              PIC  X(11)  VALUE  SPACE.
*    02  BAIKINGK3           PIC  ---,---,--9.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  HENBKSU3            PIC  ----,--9.
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  GENKINGK3           PIC  ----,---,--9.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  BAIKINGK3           PIC  ---,---,--9.
* 2023/11/20 UPD END
*    原票合計
 01  DKEI-AREA.
     02  DHBKSU              PIC  9(07)V9.
     02  DKINGK              PIC  9(09).
     02  DKINGKB             PIC  9(09).
*    店舗合計
 01  TKEI-AREA.
     02  THBKSU              PIC  9(07)V9.
     02  TKINGK              PIC  9(09).
     02  TKINGKB             PIC  9(09).
*    総合計
 01  SKEI-AREA.
     02  SHBKSU              PIC  9(07)V9.
     02  SKINGK              PIC  9(09).
     02  SKINGKB             PIC  9(09).
*
 LINKAGE                     SECTION.
 01  PAR-TORCD               PIC 9(08).
 01  PAR-SYURUI              PIC X(01).
 01  PAR-YMDFROM             PIC 9(08).
 01  PAR-YMDTO               PIC 9(08).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION USING PAR-TORCD
                                          PAR-SYURUI
                                          PAR-YMDFROM
                                          PAR-YMDTO.
 DECLARATIVES.
 PRT-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     STOP        RUN.
 HEP-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE BMSHE7W.
     DISPLAY     HEP-ERR   UPON      CONS.
     DISPLAY     HEP-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理　　　　　　　　　　　　*
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
*
     PERFORM     DEN-READ-SEC
                 UNTIL  END-SW  =   1   OR
                        HIT-FLG =   1.
     MOVE        NEW-KEY          TO        OLD-KEY.

     PERFORM     MAIN-SEC
                 UNTIL END-SW = 1.
*
     PERFORM     END-SEC.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ　　　　　　　　　　　 *
**********************************************************
 INIT-SEC                  SECTION.
     MOVE      ZERO               TO   PAGE-CNT.
     MOVE      58                 TO   LINE-CNT.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
**** MOVE      20                 TO   WK-YS.
     INITIALIZE                        DKEI-AREA
                                       TKEI-AREA
                                       SKEI-AREA.
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
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     OPEN        INPUT     BMSHE7W
                 OUTPUT    PRTF.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理　　　　　　　　　　　　*
***********************************************************
 MAIN-SEC                  SECTION.
*
     IF    NEW-KEY    NOT =  OLD-KEY
       IF  NEW-TENCD  NOT =  OLD-TENCD
*          伝票計出力
           PERFORM     DENWRT-SEC
*          店舗計出力
           PERFORM     TENWRT-SEC
       ELSE
*          伝票計出力
           PERFORM     DENWRT-SEC
       END-IF
     END-IF.
*    集計
     PERFORM     SYUKEI-SEC.
*    明細出力
     PERFORM     MEIWRT-SEC.
*    読込
     MOVE        0                TO        HIT-FLG.
     PERFORM     DEN-READ-SEC
                 UNTIL  END-SW  =   1   OR
                        HIT-FLG =   1.
*
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ　　　　　　　　　　　　 *
**********************************************************
 END-SEC                   SECTION.
*
     IF    IN-CNT      >   ZERO
*          伝票計出力
           PERFORM     DENWRT-SEC
*          店舗計出力
           PERFORM     TENWRT-SEC
*          総合計出力
           PERFORM     SOUWRT-SEC
     ELSE
     DISPLAY NC"＃＃出力対象データがありません！＃＃" UPON CONS
     END-IF.
*
     CLOSE       PRTF    BMSHE7W.
*    DISPLAY  "SSY4530L READ =" READ-CNT UPON STA.
*    DISPLAY  "SSY4530L IN   =" IN-CNT   UPON STA.
 END-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し　　　　　　　　　*
**********************************************************
 SYUKEI-SEC                   SECTION.
*
     ADD       HEP-F18            TO        DHBKSU.
     ADD       HEP-F14            TO        DKINGK.
     ADD       HEP-F16            TO        DKINGKB.
*
     ADD       HEP-F18            TO        THBKSU.
     ADD       HEP-F14            TO        TKINGK.
     ADD       HEP-F16            TO        TKINGKB.
*
     ADD       HEP-F18            TO        SHBKSU.
     ADD       HEP-F14            TO        SKINGK.
     ADD       HEP-F16            TO        SKINGKB.
*
 SYUKEI-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し　　　　　　　 *
**********************************************************
 HEAD-WRT-SEC                  SECTION.
*
     IF  PAR-SYURUI = "1"
         MOVE    NC"受信日"       TO        HITTL
         MOVE    NC"受信日"       TO        MID2-01
     ELSE
         MOVE    NC"計上日"       TO        HITTL
         MOVE    NC"計上日"       TO        MID2-01
     END-IF.
     MOVE      SYS-DATE(1:4)      TO        SYSYY.
     MOVE      WK-M               TO        SYSMM.
     MOVE      WK-D               TO        SYSDD.
*
     MOVE      HEP-F02            TO        TORINAM.
     MOVE      PAR-YMDFROM        TO        DATEFROM.
     MOVE      PAR-YMDTO          TO        DATETO.
*
     IF  PAGE-CNT  >  ZERO
         MOVE      SPACE          TO        P-REC
         WRITE     P-REC         AFTER      PAGE
     END-IF.
*
     ADD       1                  TO        PAGE-CNT.
     MOVE      ZERO               TO        LINE-CNT.
     MOVE      PAGE-CNT           TO        LPAGE.
**************
*帳票書き出し*
**************
     MOVE      SPACE              TO        P-REC.
*    WRITE     P-REC   AFTER   2.
     WRITE     P-REC   FROM    MIDASI-1     AFTER  1.
     WRITE     P-REC   FROM    MIDASI-2     AFTER  2.
     WRITE     P-REC   FROM    MIDASI-3     AFTER  1.
     WRITE     P-REC   FROM    HASEN-1      AFTER  1.
*
     WRITE     P-REC   FROM    MIDASI-4     AFTER  1.
     WRITE     P-REC   FROM    MIDASI-5     AFTER  1.
     WRITE     P-REC   FROM    HASEN-1      AFTER  1.
*
     MOVE      10                 TO        LINE-CNT.
*
 HEAD-WRT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し　　　　　　　　　*
**********************************************************
 MEIWRT-SEC                  SECTION.
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
     MOVE      SPACE              TO        MEISAI-1.
     MOVE      SPACE              TO        MEISAI-2.

*日付種類
     IF  DEN-FLG = 0
          MOVE  HEP-F04(1:2)      TO        TENCD
          MOVE  HEP-F06           TO        TENNM
*         処理種別
          MOVE  HEP-F08           TO        SYRKBN
          EVALUATE HEP-F08
             WHEN  "10"  MOVE NC"発注仕入訂"  TO  SYRKBNNM
             WHEN  "20"  MOVE NC"値引"        TO  SYRKBNNM
             WHEN  "30"  MOVE NC"返品"        TO  SYRKBNNM
             WHEN  OTHER MOVE NC"＊＊＊＊＊"  TO  SYRKBNNM
          END-EVALUATE
          MOVE  HEP-F07           TO        KEIYMD
          MOVE  HEP-F03           TO        HEN-DENNOX
          MOVE  HEN-DENNO         TO        DENNO
          MOVE  HEP-F19           TO        BUMON
     END-IF.
*
     MOVE      HEP-F09            TO        HEN-GNOX.
     MOVE      HEN-GNO            TO        GNO.
     MOVE      HEP-F11            TO        HINCD.
     MOVE      HEP-F12            TO        HINKANA.
     MOVE      HEP-F18            TO        HENBKSU.
     MOVE      HEP-F15            TO        GENTANKA.
     MOVE      HEP-F14            TO        GENKINGK.
     MOVE      HEP-F17            TO        BAITANKA.
     MOVE      HEP-F16            TO        BAIKINGK.
* 2023/11/20 ADD START
     MOVE      HEP-F21            TO        JYOTOYM1.
     MOVE      HEP-F22            TO        JYOTOYM2.
* 2023/11/20 ADD END
*
**************
*帳票書き出し*
**************
     IF  DEN-FLG   =    0
         WRITE     P-REC   FROM   MEISAI-1  AFTER  1
         MOVE      1              TO        DEN-FLG
         ADD       1              TO        LINE-CNT
     END-IF.
*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
     WRITE      P-REC   FROM      MEISAI-2  AFTER  1.
     ADD        1                 TO        LINE-CNT.
*
 MEIWRT-EXIT.
     EXIT.
**********************************************************
*                    伝票合計　書き出し　　　　　　　　　*
**********************************************************
 DENWRT-SEC                   SECTION.
*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
     MOVE      DHBKSU             TO        HENBKSU1.
     MOVE      DKINGK             TO        GENKINGK1.
     MOVE      DKINGKB            TO        BAIKINGK1.
*
**************
*帳票書き出し*
**************
     WRITE      P-REC   FROM    GOKEI-1     AFTER  1.
     ADD        1                 TO        LINE-CNT.
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
     WRITE      P-REC   FROM    HASEN-2     AFTER  1.
     ADD        1                 TO        LINE-CNT.
     MOVE       0                 TO        DEN-FLG.
     INITIALIZE                             DKEI-AREA.
     MOVE     NEW-DENNO           TO        OLD-DENNO.
*
 DENWRT-EXIT.
     EXIT.
**********************************************************
*                    店舗合計　書き出し　　　　　　　　　*
**********************************************************
 TENWRT-SEC                   SECTION.
*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
     MOVE      THBKSU             TO        HENBKSU2.
     MOVE      TKINGK             TO        GENKINGK2.
     MOVE      TKINGKB            TO        BAIKINGK2.
*
**************
*帳票書き出し*
**************
     WRITE      P-REC   FROM    GOKEI-2     AFTER  1.
     ADD        1                 TO        LINE-CNT.
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
     WRITE      P-REC   FROM    HASEN-3     AFTER  1.
     ADD        1                 TO        LINE-CNT.
     MOVE       0                 TO        DEN-FLG.
     INITIALIZE                   TKEI-AREA.
     MOVE     NEW-TENCD           TO        OLD-TENCD.
*
 TENWRT-EXIT.
     EXIT.
**********************************************************
*                    総合計　　書き出し　　　　　　　　　*
**********************************************************
 SOUWRT-SEC                   SECTION.
*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
     MOVE      SHBKSU             TO        HENBKSU3.
     MOVE      SKINGK             TO        GENKINGK3.
     MOVE      SKINGKB            TO        BAIKINGK3.
*
**************
*帳票書き出し*
**************
     WRITE      P-REC   FROM    GOKEI-3    AFTER  1.
*
 SOUWRT-EXIT.
     EXIT.
****************************************************************
*             流通ＢＭＳ受領書ワーク読込み　　　　　　2.0      *
****************************************************************
 DEN-READ-SEC            SECTION.
*
     READ     BMSHE7W  NEXT  AT  END
              MOVE     1         TO   END-SW
              MOVE     99999999  TO   NEW-TENCD
              MOVE     9999999   TO   NEW-DENNO
              GO                 TO   DEN-READ-EXIT
     END-READ.
*
     IF       HEP-F013           =    PAR-TORCD
        IF    PAR-SYURUI         =   "1"
          IF (HEP-F011           >=   PAR-YMDFROM   AND
              HEP-F011           <=   PAR-YMDTO)
              MOVE     HEP-F04(1:2)   TO   NEW-TENCD
              MOVE     HEP-F03(1:9)   TO   NEW-DENNO
              ADD      1              TO   IN-CNT
              MOVE     1              TO   HIT-FLG
          END-IF
        ELSE
          IF (HEP-F07            >=   PAR-YMDFROM   AND
              HEP-F07            <=   PAR-YMDTO)
              MOVE     HEP-F04(1:2)   TO   NEW-TENCD
              MOVE     HEP-F03(1:9)   TO   NEW-DENNO
              ADD      1              TO   IN-CNT
              MOVE     1              TO   HIT-FLG
          END-IF
        END-IF
     END-IF.
*
 DEN-READ-EXIT.
     EXIT.
