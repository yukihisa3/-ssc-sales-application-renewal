****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　流通ＢＭＳ　　　　　　　　　　　　*
*    モジュール名　　　　：　支払明細書発行　　　　　　　　　　*
*                              （ＨＩヒロセ）　　　　　　　　　*
*    作成日／作成者　　　：　2024/06/25                        *
*    処理概要　　　　　　：　流通ＢＭＳ支払ワークより　　　　　*
*                        ：　支払明細書を発行する。　　　　　　*
*                        　　PENDING:店明細以外の伝票番号不明
*    更新履歴　　　　　　：　                                  *
*                            　　　　　　　　　　　　　　　　  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SBM0222L.
*                      流用：SBM0213L (ジュンテンドー)
 AUTHOR.                     NAV.
 DATE-WRITTEN.               2024/06/25.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
              YA        IS   NIHONGO
              YB        IS   YB
              YB-21     IS   YB-21
              YB-22     IS   YB-22
              CONSOLE   IS   CONS.
******************************************************************
 INPUT-OUTPUT           SECTION.
******************************************************************
 FILE-CONTROL.
******************************************************************
***  支払ワーク（ＨＩヒロセ）
     SELECT  BMSSH8W1   ASSIGN    TO        DA-01-VI-BMSSH8W1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SIH-F013
                                            SIH-F308
                                            SIH-F405
                                            SIH-F415
                                            SIH-F402
                        FILE      STATUS    SIH-ST.
***  取引先マスタ
     SELECT  HTOKMS     ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-ST.
***  店舗マスタ
     SELECT  HTENMS     ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE      STATUS    TEN-ST.
***  プリンター
     SELECT   PRTF      ASSIGN    TO        LP-04
                        FILE      STATUS    PRT-ST.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
******************************************************************
***  支払ワーク
 FD  BMSSH8W1.
     COPY     BMSSH8W1            OF   XFDLIB
              JOINING   SIH       AS   PREFIX.
***  取引先マスタ
 FD  HTOKMS.
     COPY     HTOKMS              OF   XFDLIB
              JOINING   TOK       AS   PREFIX.
***  店舗マスタ
 FD  HTENMS.
     COPY     HTENMS              OF   XFDLIB
              JOINING   TEN       AS   PREFIX.
***  プリンター
 FD  PRTF     LINAGE         IS   66.
 01  P-REC                   PIC  X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
 01  FILE-STATUS.
     03  SIH-ST              PIC  X(02).
     03  TOK-ST              PIC  X(02).
     03  TEN-ST              PIC  X(02).
     03  PRT-ST              PIC  X(02).
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
 01  WK-SIH-F402             PIC  X(06).
 01  WK-SIH-F402-R     REDEFINES      WK-SIH-F402.
     03  WK-SIH-F402-H       PIC  9(06).
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
     03  SIH-ERR             PIC  N(10) VALUE
         NC"支払ワークエラー".
     03  TOK-ERR             PIC  N(10) VALUE
         NC"取引先マスタエラー".
     03  TEN-ERR             PIC  N(10) VALUE
         NC"店舗マスタエラー".
     03  PRT-ERR             PIC  N(10) VALUE
         NC"プリンターエラー".
*
 01  IX                      PIC  9(04) VALUE  0.
 01  READ-CNT                PIC  9(07) VALUE  0.
 01  IN-CNT                  PIC  9(07) VALUE  0.
 01  SET-FLG                 PIC  X(03) VALUE  SPACE.
 01  HENKAN-FLG              PIC  X(03) VALUE  SPACE.
 01  TEN-FLG                 PIC  9(01) VALUE  0.
 01  HIT-FLG                 PIC  9(01) VALUE  0.
*
 01  NEW-KEY.
     03  NEW-TORICD          PIC  9(08).
     03  NEW-SIMEBI          PIC  9(08).
     03  NEW-TENCD           PIC  9(05).
 01  WK-TENCDR.
     03  WK-TENCD            PIC  9(03).
 01  OLD-KEY.
     03  OLD-TORICD          PIC  9(08).
     03  OLD-SIMEBI          PIC  9(08).
     03  OLD-TENCD           PIC  9(05).
*
 01  WK-YMD.
     03  WK-YY               PIC  9999.
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  WK-MM               PIC  99.
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  WK-DD               PIC  99.
*
 01  WK-KEIYMD               PIC  9(08)  VALUE  ZERO.
 01  WK-KINGK                PIC S9(10)  VALUE  ZERO.
*    店舗合計
 01  TKEI-AREA.
     03  TKEI-KINGK          PIC S9(10).
*    総合計
 01  SKEI-AREA.
     03  SKEI-KINGK          PIC S9(10).
*
*帳票出力定義エリア
****  見出し行１　　　　　　 ****
 01  MIDASI-1.
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  X(08)  VALUE  "SBM0222L".
     03  FILLER              PIC  X(30)  VALUE  SPACE.
     03  FILLER              PIC  N(13)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＜　支　払　明　細　書　＞".
     03  FILLER              PIC  X(27)  VALUE  SPACE.
     03  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  YB         VALUE
         NC"作成日：".
     03  SYSYY               PIC  9999.
     03  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"年".
     03  SYSMM               PIC  Z9.
     03  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"月".
     03  SYSDD               PIC  Z9.
     03  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"日".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  LPAGE               PIC  ZZZ9.
     03  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"頁".
****  見出し行２***
 01  MIDASI-2.
     03  FILLER              PIC  X(49)  VALUE  SPACE.
     03  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"＜締日：".
     03  SIMEY               PIC  9999.
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  SIMEM               PIC  99.
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  SIMED               PIC  99.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"＞".
****  見出し行３***
 01  MIDASI-3.
     03  FILLER              PIC  X(106)  VALUE  SPACE.
     03  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  YB          VALUE
         NC"　締日：".
     03  SIMEY2              PIC  9999.
     03  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"年".
     03  SIMEM2              PIC  Z9.
     03  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"月".
     03  SIMED2              PIC  Z9.
     03  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"日".
****  見出し行４***
 01  MIDASI-4.
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"取引先：".
     03  TORICD              PIC  9(06).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  TORINM              PIC  N(16)
         CHARACTER  TYPE  IS  YB.
****  見出し行５***
 01  MIDASI-5.
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(07)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"店　舗　情　報".
     03  FILLER              PIC  X(11)  VALUE  SPACE.
     03  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"計上日".
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"伝票種別".
     03  FILLER              PIC  X(08)  VALUE  SPACE.
     03  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"照合結果".
     03  FILLER              PIC  X(17)  VALUE  SPACE.
     03  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"伝票番号".
     03  FILLER              PIC  X(07)  VALUE  SPACE.
     03  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"支払金額".
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"支払内容".
     03  FILLER              PIC  X(20)  VALUE  SPACE.
     03  FILLER              PIC  N(02)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"税率".
****  線１　　　　 ****
 01  HASEN-1.
     03  FILLER              PIC  X(136) VALUE  ALL "=".
****  線３　　　　 ****
 01  HASEN-3.
     03  FILLER              PIC  X(136) VALUE  ALL "-".
*
****  明細行１　　 ****
 01  MEISAI-1.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  TENCD               PIC  999.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  TENNM               PIC  N(10)
                             CHARACTER  TYPE  IS  YB.
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  KEIYMD              PIC  X(10).
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  DENSBT              PIC  X(04).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  DENSNM              PIC  N(06)
                             CHARACTER  TYPE  IS  YB.
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  KEKACD              PIC  X(02).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  KEKANM              PIC  N(14)
                             CHARACTER  TYPE  IS  YB.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  DENNO               PIC  X(06).
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  SIHKINGK            PIC  -,---,---,--9.
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  SIHNNO              PIC  X(04).
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  SIHNNM              PIC  N(14)
                             CHARACTER  TYPE  IS  YB.
     03  FILLER              PIC  X(02)  VALUE  SPACE.
     03  ZEIRITSU            PIC  ZZ.Z.
*
****  店舗計　　　 ****
 01  GOKEI-1.
     03  FILLER              PIC  X(64)  VALUE  SPACE.
     03  FILLER              PIC  N(08)  VALUE
         NC"＜店舗支払合計＞"     CHARACTER  TYPE  IS  NIHONGO.
     03  FILLER              PIC  X(06)  VALUE  SPACE.
     03  GK1KINGK            PIC  --,---,---,--9.
*
****  総合計　　　 ****
 01  GOKEI-S.
     03  FILLER              PIC  X(64)  VALUE  SPACE.
     03  FILLER              PIC  N(08)  VALUE
         NC"＜支払総合計＞　"     CHARACTER  TYPE  IS  NIHONGO.
     03  FILLER              PIC  X(06)  VALUE  SPACE.
     03  GKSKINGK            PIC  --,---,---,--9.
 01  GOKEI-S1.
     03  FILLER              PIC  X(35)  VALUE  SPACE.
     03  FILLER              PIC  N(06)  VALUE
         NC"□登録番号→"     CHARACTER  TYPE  IS  NIHONGO.
     03  FILLER              PIC  X(05)  VALUE  SPACE.
     03  FILLER              PIC  N(07)  VALUE
         NC"＜ＨＩヒロセ＞"   CHARACTER  TYPE  IS  NIHONGO.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  TOUROKNO1           PIC  X(14).
     03  FILLER              PIC  X(04)  VALUE  SPACE.
     03  FILLER              PIC  N(08)  VALUE
         NC"＜サカタのタネ＞"     CHARACTER  TYPE  IS  NIHONGO.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  TOUROKNO2           PIC  X(14).
*
*    登録番号セーブ
 01  SAVE-AREA.
     02  SV-TOUROKU1         PIC  X(14).
     02  SV-TOUROKU2         PIC  X(14).
*
******************************************************************
 PROCEDURE              DIVISION.
******************************************************************
 DECLARATIVES.
 PRT-ERR-EXC            SECTION.
     USE      AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY  PRT-ERR   UPON      CONS.
     DISPLAY  PRT-ST    UPON      CONS.
     MOVE     4000      TO        PROGRAM-STATUS.
     STOP     RUN.
 SIH-ERR-EXC            SECTION.
     USE      AFTER     EXCEPTION PROCEDURE BMSSH8W1.
     DISPLAY  SIH-ERR   UPON      CONS.
     DISPLAY  SIH-ST    UPON      CONS.
     MOVE     4000      TO        PROGRAM-STATUS.
     STOP     RUN.
 TOK-ERR-EXC            SECTION.
     USE      AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY  TOK-ERR   UPON      CONS.
     DISPLAY  TOK-ST    UPON      CONS.
     MOVE     4000      TO        PROGRAM-STATUS.
     STOP     RUN.
 TEN-ERR-EXC            SECTION.
     USE      AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY  TEN-ERR   UPON      CONS.
     DISPLAY  TEN-ST    UPON      CONS.
     MOVE     4000      TO        PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
***************************************************************
*    PGM CONTROL                                              *
***************************************************************
 PROC-SEC               SECTION.
*
     PERFORM  INIT-SEC.
*
     PERFORM  SIH-READ-SEC
              UNTIL     END-SW    =   1   OR
                        HIT-FLG   =   1.
     MOVE     NEW-KEY             TO   OLD-KEY.
*
     PERFORM  MAIN-SEC
              UNTIL     END-SW    =   1.
*
     PERFORM  END-SEC.
*
     STOP     RUN.
 PROC-EXIT.
     EXIT.
***************************************************************
*    初期処理
***************************************************************
 INIT-SEC               SECTION.
     MOVE     ZERO                TO   PAGE-CNT.
     MOVE     58                  TO   LINE-CNT.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     INITIALIZE                        TKEI-AREA
                                       SKEI-AREA.
*
*    登録番号セーブエリア
     INITIALIZE                        SAVE-AREA.
*
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
     MOVE     LINK-OUT-YMD        TO   DATE-AREA.
*システム時刻取得
     ACCEPT   WK-TIME           FROM   TIME.
*
     OPEN     INPUT     BMSSH8W1   HTOKMS   HTENMS
              OUTPUT    PRTF.
*
 INIT-EXIT.
     EXIT.
***************************************************************
*    主処理
***************************************************************
 MAIN-SEC               SECTION.
*
*店舗計出力
     IF       NEW-KEY        NOT  =    OLD-KEY
              PERFORM   TENWRT-SEC
     END-IF.
*取引先／締日
     IF     ( NEW-TORICD     NOT  =    OLD-TORICD )  OR
            ( NEW-SIMEBI     NOT  =    OLD-SIMEBI )
              MOVE      NEW-KEY            TO   OLD-KEY
              PERFORM   HEAD-WRT-SEC
     END-IF.
*集計
     PERFORM  SYUKEI-SEC.
*明細出力
     PERFORM  MEIWRT-SEC.
     MOVE     NEW-KEY             TO   OLD-KEY.
*読込
     MOVE     0                   TO   HIT-FLG.
     PERFORM  SIH-READ-SEC
              UNTIL     END-SW    =    1  OR
                        HIT-FLG   =    1.
*
 MAIN-EXIT.
     EXIT.
***************************************************************
*    終了処理
***************************************************************
 END-SEC                SECTION.
*
     IF       IN-CNT         >    ZERO
*******IF     OLD-TENCD NOT = "99999"
*店舗計出力
              PERFORM   TENWRT-SEC
*総合計出力
              PERFORM   SOUWRT-SEC
*******END-IF
       WRITE  P-REC     FROM HASEN-3  AFTER  1
     END-IF.
*
     DISPLAY  NC"出力明細件数＝"     IN-CNT    UPON   CONS.
     CLOSE    PRTF      BMSSH8W1   HTOKMS    HTENMS.
*
 END-EXIT.
     EXIT.
***************************************************************
*    金額集計
***************************************************************
 SYUKEI-SEC             SECTION.
*
     IF       SIH-F420       =    "-"
              COMPUTE   WK-KINGK  =    SIH-F419  *  -1
     ELSE
              MOVE      SIH-F419       TO   WK-KINGK
     END-IF.
*
 SYUKEI-EXIT.
     EXIT.
***************************************************************
*    見出しデータ編集書き出し
***************************************************************
 HEAD-WRT-SEC           SECTION.
*
     IF       PAGE-CNT       >    ZERO
              MOVE     SPACE           TO   P-REC
              WRITE    P-REC      AFTER     PAGE
     END-IF.
*
     MOVE     SYS-DATE(1:4)       TO   SYSYY.
     MOVE     WK-M                TO   SYSMM.
     MOVE     WK-D                TO   SYSDD.
*
     MOVE     OLD-SIMEBI(1:4)     TO   SIMEY.
     MOVE     OLD-SIMEBI(5:2)     TO   SIMEM.
     MOVE     OLD-SIMEBI(7:2)     TO   SIMED.
*
     MOVE     OLD-SIMEBI(1:4)     TO   SIMEY2.
     MOVE     OLD-SIMEBI(5:2)     TO   SIMEM2.
     MOVE     OLD-SIMEBI(7:2)     TO   SIMED2.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     ZERO                TO   LINE-CNT.
     MOVE     PAGE-CNT            TO   LPAGE.
*取引先名
     MOVE     OLD-TORICD          TO   TORICD    TOK-F01.
     READ     HTOKMS
              INVALID
              MOVE     SPACE           TO   TOK-F02
     END-READ.
     MOVE     TOK-F02             TO   TORINM.
*
     MOVE     SPACE               TO   P-REC.
     WRITE    P-REC     FROM      MIDASI-1  AFTER  1.
     WRITE    P-REC     FROM      MIDASI-2  AFTER  1.
     WRITE    P-REC     FROM      MIDASI-3  AFTER  1.
     WRITE    P-REC     FROM      MIDASI-4  AFTER  1.
*
     WRITE    P-REC     FROM      HASEN-1   AFTER  1.
     WRITE    P-REC     FROM      MIDASI-5  AFTER  1.
     WRITE    P-REC     FROM      HASEN-1   AFTER  1.
*
     MOVE     8                   TO   LINE-CNT.
     MOVE     ZERO                TO   TEN-FLG.
*
 HEAD-WRT-EXIT.
     EXIT.
***************************************************************
*    明細データ書き出し
***************************************************************
 MEIWRT-SEC             SECTION.
     IF       LINE-CNT       >    57
              PERFORM   HEAD-WRT-SEC
     END-IF.
*
     MOVE     SPACE          TO   MEISAI-1.
*ヘッダ
     IF       TEN-FLG        =    0
*      店舗情報
              IF   SIH-F405(1:3)  =    "999"
                   MOVE   NC"相殺又は支払合計"   TO   TENNM
              ELSE
                   MOVE   SIH-F405(1:3)          TO   WK-TENCDR
                   MOVE   WK-TENCD               TO   TENCD
                                                      TEN-F011
                   MOVE   SIH-F013               TO   TEN-F52
                   READ   HTENMS
                          INVALID
                          MOVE   ALL NC"＊"      TO   TEN-F03
                   END-READ
                   MOVE   TEN-F03                TO   TENNM
              END-IF
*      計上日
              MOVE      SIH-F415(1:4)       TO   WK-YY
              MOVE      SIH-F415(5:2)       TO   WK-MM
              MOVE      SIH-F415(7:2)       TO   WK-DD
              MOVE      WK-YMD              TO   KEIYMD
              MOVE      1                   TO   TEN-FLG
     END-IF.
*明細
*    計上日
     IF       SIH-F415  NOT  =    WK-KEIYMD
              MOVE      SIH-F415(5:2)       TO   WK-MM
              MOVE      SIH-F415(7:2)       TO   WK-DD
              MOVE      WK-YMD              TO   KEIYMD
              MOVE      SIH-F415            TO   WK-KEIYMD
     END-IF.
*    伝票種別（支払内容）
     MOVE     SIH-F428            TO   DENSBT.
     EVALUATE      SIH-F428
         WHEN      "1001"
                   MOVE  NC"仕入明細"       TO   DENSNM
         WHEN      "1002"
                   MOVE  NC"返品明細"       TO   DENSNM
         WHEN      "1004"
                   MOVE  NC"値引明細"       TO   DENSNM
         WHEN      "2000"
                   MOVE  NC"相殺明細"       TO   DENSNM
         WHEN      "3001"
                   MOVE  NC"相殺前支払額"   TO   DENSNM
         WHEN      "3002"
                   MOVE  NC"相殺合計"       TO   DENSNM
         WHEN      "3003"
                   MOVE  NC"支払額"         TO   DENSNM
     END-EVALUATE.
*    照合結果
     MOVE     SIH-F427            TO   KEKACD.
     MOVE     SPACE               TO   KEKANM.
     EVALUATE SIH-F427
         WHEN "00"
              MOVE  SPACE                          TO   KEKANM
         WHEN "01"
              MOVE  NC"請求照合分支払"             TO   KEKANM
         WHEN "02"
              MOVE  NC"請求のない返品・値引・割戻" TO   KEKANM
         WHEN "03"
              MOVE  NC"再調査請う"                 TO   KEKANM
         WHEN "04"
              MOVE  NC"計上払対象"                 TO   KEKANM
         WHEN "21"
              MOVE  NC"請求照合分支払（再請求分）" TO   KEKANM
         WHEN "22"
              MOVE  NC"支払期間対象外"             TO   KEKANM
         WHEN OTHER
              MOVE  NC"＊＊＊＊＊＊＊＊＊＊＊＊＊" TO   KEKANM
     END-EVALUATE.
*    伝票番号
     MOVE     SIH-F402(1:6)       TO   WK-SIH-F402.
     MOVE     WK-SIH-F402-H       TO   DENNO.
*    PENDING:店明細以外の伝票番号不明
     IF       SIH-F402(1:6)       =    "0     "
              MOVE   SPACE        TO   DENNO
     END-IF.
     IF       SIH-F402(1:6)       =    "000000"
              MOVE   SPACE        TO   DENNO
     END-IF.
*    支払金額
     MOVE     WK-KINGK            TO   SIHKINGK.
     ADD      WK-KINGK            TO   TKEI-KINGK.
     ADD      WK-KINGK            TO   SKEI-KINGK.
*    支払内容（支払内容個別）
     IF     ( SIH-F429  NOT  =    "0000" )   AND
            ( SIH-F429  NOT  =    "    " )
              MOVE      SIH-F429       TO   SIHNNO
     END-IF.
*    支払内容（支払内容個別名称
     MOVE     SIH-F430            TO   SIHNNM.
*    税率
     MOVE     SIH-F434            TO   ZEIRITSU.
*
     WRITE    P-REC          FROM MEISAI-1  AFTER  1.
     ADD      1              TO   LINE-CNT.
*
 MEIWRT-EXIT.
     EXIT.
***************************************************************
*    店舗合計　書き出し
***************************************************************
 TENWRT-SEC             SECTION.
*
     IF       LINE-CNT       >    57
              PERFORM   HEAD-WRT-SEC
     END-IF.
     MOVE     TKEI-KINGK         TO   GK1KINGK.
*
     WRITE    P-REC          FROM GOKEI-1   AFTER  2.
     ADD      2              TO   LINE-CNT.
     IF       LINE-CNT       >    57
              PERFORM   HEAD-WRT-SEC
     END-IF.
*
     WRITE    P-REC          FROM HASEN-3  AFTER  1.
     ADD      1              TO   LINE-CNT.
     MOVE     0              TO   TEN-FLG.
     INITIALIZE                   TKEI-AREA.
     MOVE     NEW-TENCD      TO   OLD-TENCD.
*
 TENWRT-EXIT.
     EXIT.
***************************************************************
*    総合計　書き出し
***************************************************************
 SOUWRT-SEC             SECTION.
*
     IF       LINE-CNT       >    57
              PERFORM   HEAD-WRT-SEC
     END-IF.
     MOVE     SKEI-KINGK     TO   GKSKINGK.
*
     MOVE     SV-TOUROKU1    TO   TOUROKNO1.
     MOVE     SV-TOUROKU2    TO   TOUROKNO2.
*
*
     WRITE    P-REC          FROM GOKEI-S   AFTER  2.
     WRITE    P-REC          FROM GOKEI-S1  AFTER  1.
*
 SOUWRT-EXIT.
     EXIT.
****************************************************************
*    支払ワーク読込み
****************************************************************
 SIH-READ-SEC           SECTION.
*
     READ     BMSSH8W1   NEXT AT  END
              MOVE      1              TO   END-SW
              MOVE      99999999       TO   NEW-TORICD
              GO   TO   SIH-READ-EXIT
     END-READ.
*
     MOVE     SIH-F013            TO   NEW-TORICD.
     MOVE     SIH-F308            TO   NEW-SIMEBI.
     MOVE     SIH-F405(1:3)       TO   WK-TENCDR.
     MOVE     WK-TENCD            TO   NEW-TENCD.
     ADD      1                   TO   IN-CNT.
     MOVE     1                   TO   HIT-FLG.
*
*    登録番号セーブ
     IF      (SIH-F428             =   "9000"    AND
              SIH-F429             =   "0002")
              MOVE  SIH-F431       TO  SV-TOUROKU1
     END-IF.
*    登録番号セーブ
     IF      (SIH-F428             =   "9000"    AND
              SIH-F429             =   "0001")
              MOVE  SIH-F431       TO  SV-TOUROKU2
     END-IF.
*
*
 SIH-READ-EXIT.
     EXIT.
