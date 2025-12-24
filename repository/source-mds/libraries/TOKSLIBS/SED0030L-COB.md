# SED0030L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SED0030L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩＣ　　　　　　　　　　　　　*
*    モジュール名　　　　：　支払明細書発行　　　　　　　　　　*
*    作成日／更新日　　　：　2015/08/25                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＥＤＩＣ支払明細書ワークよりＥＤ　*
*                        ：　ＩＣ支払明細書を出力する　　　　　*
*    更新日／更新者　　　：　2015/11/11 NAV TAKAHASHI          *
*    更新概要　　　　　　：　合計出力方法を変更　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION            DIVISION.
****************************************************************
 PROGRAM-ID.               SED0030L.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2015/08/25.
****************************************************************
 ENVIRONMENT               DIVISION.
****************************************************************
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
****************************************************************
 INPUT-OUTPUT           SECTION.
****************************************************************
 FILE-CONTROL.
*----<< EDIC支払明細書ワーク >>--*
     SELECT     EWSIHAF    ASSIGN    TO        DA-01-VI-EWSIHAL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       SWK-F01
                                               SWK-F05
                                               SWK-F08
                                               SWK-F04
                           FILE      STATUS    SWK-ST.
*----<< ﾌﾟﾘﾝﾀｰ >>--*
     SELECT     PRTF       ASSIGN    TO        LP-04-PRTF.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
*----<< EDIC支払明細書ワーク >>--*
 FD  EWSIHAF     LABEL     RECORD    IS        STANDARD.
     COPY        EWSIHAF   OF        XFDLIB
     JOINING     SWK       AS        PREFIX.
*
*----<< ﾌﾟﾘﾝﾀｰ >>--*
 FD    PRTF      LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
*
 01  FILE-STATUS.
     03  PRT-ST              PIC  X(02).
     03  PRT-ST1             PIC  X(04).
     03  SWK-ST              PIC  X(02).
 01  WK-AREA.
     03  END-SW              PIC  9(01)  VALUE   ZERO.
     03  ERR-SW              PIC  9(01)  VALUE   ZERO.
     03  PAGE-CNT            PIC  9(05)  VALUE   ZERO.
     03  LINE-CNT            PIC  9(05)  VALUE   ZERO.
     03  END-FLG             PIC  X(03)  VALUE   SPACE.
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
*
*日付時刻メッセージ表示用
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
*----<< 店舗コード変換 >>--*
 01  TEMPOCD-MOJI            PIC  X(05).
 01  FILLER            REDEFINES      TEMPOCD-MOJI.
     03  TEMPOCD-SUCHI       PIC  9(05).
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
     03  SWK-ERR             PIC  N(10) VALUE
         NC"帳票ワークエラー".
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
*----<< 印刷制御用変数 >>--*
 01  NEW-TENCD          PIC  X(05)  VALUE  SPACE.
 01  OLD-TENCD          PIC  X(05)  VALUE  SPACE.
 01  NEW-KEJOBI         PIC  X(08)  VALUE  SPACE.
 01  OLD-KEJOBI         PIC  X(08)  VALUE  SPACE.
*
 01  HEN-GNOX.
     02  HEN-GNO             PIC  9(02).
     02  FILLER              PIC  X(02).
*
 01  HEN-DENNOX.
     02  HEN-DENNO           PIC  9(07).
     02  FILLER              PIC  X(03).
*
*帳票出力定義エリア
****　見出し行１
 01  MIDASI-1.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SED0030L".
     02  FILLER              PIC  X(43)  VALUE  SPACE.
     02  FILLER              PIC  N(08)
         CHARACTER  TYPE  IS  YB-22 VALUE  NC"＜支払明細書＞".
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
*
****  見出し行２
 01  MIDASI-2.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"締　日：".
     02  JUSHINBI-NEN        PIC  9(04).
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"年".
     02  JUSHINBI-TUKI       PIC  9(02).
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"月".
     02  JUSHINBI-HI         PIC  9(02).
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"日".
*
****  見出し行３
 01  MIDASI-3.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"取引先：".
     02  TORICD              PIC  9(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TORINAM             PIC  N(16)
         CHARACTER  TYPE  IS  YB.
*
****  見出し行４
 01  MIDASI-4.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(07)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"店　舗　情　報".
     02  FILLER              PIC  X(11)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"計上日".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"伝票番号".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"支払金額".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"照合結果".
     02  FILLER              PIC  X(16)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"支払対象".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"支払内容".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(06)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"（個別内容）".
*
****  線１
 01  HASEN-1.
     02  FILLER              PIC  X(136) VALUE  ALL "=".
****  線２
 01  HASEN-2.
     02  FILLER              PIC  X(32) VALUE  SPACE.
     02  FILLER              PIC  X(104) VALUE  ALL "-".
****  線３
 01  HASEN-3.
     02  FILLER              PIC  X(136) VALUE  ALL "-".
*
****  明細行１
 01  MEISAI-1.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  TENCD               PIC  ZZZZ9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TENNM               PIC  N(10)
         CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  KEIJOBI-NEN         PIC  9(04).
     02  KUGIRI1             PIC  X(01)  VALUE  "/".
     02  KEIJOBI-TUKI        PIC  9(02).
     02  KUGIRI2             PIC  X(01)  VALUE  "/".
     02  KEIJOBI-HI          PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  DEMPYONO            PIC  9(09).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHIHARAIKINGAKU     PIC  -,---,---,--9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHOGOKEKKA          PIC  X(02).
     02  SHOGONAIYO          PIC  N(14)
         CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHIHARAITAISHO      PIC  N(06)
         CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHIHARAINAIYO       PIC  X(04).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHIHARAINAIYOMEI    PIC  N(06)
         CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  KOBETSUNAIYO        PIC  X(25).
*
****  店舗計
 01  GOKEI-1.
     02  FILLER              PIC  X(28)  VALUE  SPACE.
     02  FILLER              PIC  N(08)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"＜店舗支払合計＞".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHIHARAIKINGAKU1    PIC  -,---,---,--9.
*
****  総合計
 01  GOKEI-2.
     02  FILLER              PIC  X(26)  VALUE  SPACE.
     02  FILLER              PIC  N(09)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"＜店舗支払総合計＞".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHIHARAIKINGAKU2    PIC  -,---,---,--9.
*
 01  TKEI-AREA.
     02  TKINGK              PIC S9(10).
*
 01  SKEI-AREA.
     02  SKINGK              PIC S9(10).
*
****************************************************************
 PROCEDURE                 DIVISION.
****************************************************************
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     STOP        RUN.
 SWK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE EWSIHAF.
     DISPLAY     SWK-ERR   UPON      CONS.
     DISPLAY     SWK-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                     ＭＡＩＮ処理
****************************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
*
     PERFORM     MAIN-SEC
                 UNTIL     END-FLG = "END".
*
     PERFORM     END-SEC.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
****************************************************************
*                      Ｉ Ｎ Ｉ Ｔ
****************************************************************
 INIT-SEC                  SECTION.
*
*----<< 開始メッセージ出力 >>--*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SED0030L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
*----<< ｺﾃｲﾁ ｾｯﾄ >--*
     MOVE      58                 TO   LINE-CNT.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE      ZERO               TO   PAGE-CNT.
*
*----<< ｼｽﾃﾑﾋﾂﾞｹ ｼｭﾄｸ >>--*
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE      20                 TO   WK-YS.
     INITIALIZE                        TKEI-AREA
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
*
*----<< ｼｽﾃﾑｼﾞｺｸ ｼｭﾄｸ >>--*
     ACCEPT    WK-TIME          FROM   TIME.
*
*----<<FILE OPEN >>--*
     OPEN        INPUT     EWSIHAF
                 OUTPUT    PRTF.
*
*----<< ワーク読込 >>--*
     PERFORM  900-WK-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                     ＭＡＩＮ処理
****************************************************************
 MAIN-SEC                  SECTION.
*
*    明細出力
     PERFORM     MEIWRT-SEC.
*
*    集計
     PERFORM     SYUKEI-SEC.
*
*    読込
     PERFORM     900-WK-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*                       Ｅ Ｎ Ｄ
****************************************************************
 END-SEC                   SECTION.
*
     IF    IN-CNT      >   ZERO
*          店舗計出力
           PERFORM     TENWRT-SEC
*          総合計出力
           PERFORM     SOUWRT-SEC
     END-IF.
*
*----<<FILE CLOSE >>--*
     CLOSE       EWSIHAF.
     CLOSE       PRTF.
*
*----<< 件数出力 >>--*
     DISPLAY  "*** INPUT  = " IN-CNT " ***" UPON CONS.
*
*----<< 終了メッセージ出力 >>--*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SED0030L END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 END-EXIT.
     EXIT.
****************************************************************
*                    明細データ書き出し
****************************************************************
 SYUKEI-SEC                   SECTION.
*
*----<< ﾃﾝﾎﾟｹｲ >>--*
     ADD       SWK-F28            TO        TKINGK.
*
*----<< ｿｳｺﾞｳｹｲ >>--*
*## 2015/11/11 NAV ST
**** ADD       SWK-F28            TO        SKINGK.
     IF  SWK-F05 NOT = "9999999999999"
         ADD   SWK-F28            TO        SKINGK
     END-IF.
*## 2015/11/11 NAV ED
*
 SYUKEI-EXIT.
     EXIT.
****************************************************************
*                 見出しデータ編集書き出し
****************************************************************
 HEAD-WRT-SEC                  SECTION.
*
*----<< ﾋﾂﾞｹ ｼﾞｶﾝ >>--*
*    MOVE      WK-YS              TO        YY1.
*    MOVE      WK-Y               TO        YY2.
     MOVE      LINK-IN-YMD8(1:4)  TO        SYSYY.
     MOVE      WK-M               TO        SYSMM.
     MOVE      WK-D               TO        SYSDD.
*    MOVE      WK-TIME(1:2)       TO        TIMEHH.
*    MOVE      WK-TIME(3:2)       TO        TIMEMM.
*    MOVE      WK-TIME(5:2)       TO        TIMESS.
*
*----<< 見出し２ >>--*
     MOVE      SWK-F03(1:4)       TO        JUSHINBI-NEN.
     MOVE      SWK-F03(5:2)       TO        JUSHINBI-TUKI.
     MOVE      SWK-F03(7:2)       TO        JUSHINBI-HI.
*
*----<< 見出し３ >>--*
     MOVE      SWK-F01            TO        TORICD.
     MOVE      SWK-F02            TO        TORINAM.
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
     WRITE     P-REC   FROM    MIDASI-4     AFTER  1.
     WRITE     P-REC   FROM    HASEN-1      AFTER  1.
*
     MOVE      10                 TO        LINE-CNT.
*
 HEAD-WRT-EXIT.
     EXIT.
****************************************************************
*                    明細データ書き出し
****************************************************************
 MEIWRT-SEC                  SECTION.
*
*----<< 改ページ判定 >>--*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
*----<< 変数クリア >>--*
     MOVE      SPACE              TO        MEISAI-1.
*
*----<< 店舗コードのブレイク判定 >>--*
     IF    NEW-TENCD  NOT =  OLD-TENCD
           IF  IN-CNT  >  1
               PERFORM     TENWRT-SEC
               MOVE  ZERO         TO        TKINGK
           END-IF
           MOVE  SWK-F05(9:5)     TO        TEMPOCD-MOJI
                                            OLD-TENCD
           MOVE  TEMPOCD-SUCHI    TO        TENCD
           MOVE  SWK-F06          TO        TENNM
           MOVE  SPACE            TO        OLD-KEJOBI
     END-IF.
*
*----<< 計上日のブレイク判定 >>--*
     IF    NEW-KEJOBI  NOT =  OLD-KEJOBI
           MOVE  SWK-F08(1:4)     TO        KEIJOBI-NEN
           MOVE  SWK-F08(5:2)     TO        KEIJOBI-TUKI
           MOVE  SWK-F08(7:2)     TO        KEIJOBI-HI
           MOVE  "/"              TO        KUGIRI1
           MOVE  "/"              TO        KUGIRI2
           MOVE  SWK-F08          TO        OLD-KEJOBI
     END-IF.
*
     MOVE      SWK-F04            TO        DEMPYONO.
     MOVE      SWK-F28            TO        SHIHARAIKINGAKU.
     MOVE      SWK-F15            TO        SHOGOKEKKA.
     MOVE      SWK-F16            TO        SHOGONAIYO.
     MOVE      SWK-F17            TO        SHIHARAITAISHO.
     MOVE      SWK-F18            TO        SHIHARAINAIYO.
     MOVE      SWK-F19            TO        SHIHARAINAIYOMEI.
     MOVE      SWK-F20            TO        KOBETSUNAIYO.
*
**************
*帳票書き出し
**************
     WRITE     P-REC   FROM   MEISAI-1  AFTER  1
     MOVE      1              TO        DEN-FLG
     ADD       1              TO        LINE-CNT
*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
 MEIWRT-EXIT.
     EXIT.
****************************************************************
*                    店舗合計書き出し
****************************************************************
 TENWRT-SEC                   SECTION.
*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
     MOVE      TKINGK             TO        SHIHARAIKINGAKU1.
*
**************
*帳票書き出し
**************
     WRITE      P-REC   FROM    GOKEI-1     AFTER  1.
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
****************************************************************
*                    総合計書き出し
****************************************************************
 SOUWRT-SEC                   SECTION.
*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
     MOVE      SKINGK             TO        SHIHARAIKINGAKU2.
*
**************
*帳票書き出し
**************
     WRITE      P-REC   FROM    GOKEI-2    AFTER  1.
*
 SOUWRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ワーク読込み
*--------------------------------------------------------------*
 900-WK-READ            SECTION.
     READ     EWSIHAF
       AT END
              MOVE     "END"           TO   END-FLG
       NOT AT END
              MOVE     SWK-F05(9:5)    TO   NEW-TENCD
              MOVE     SWK-F08         TO   NEW-KEJOBI
              ADD       1              TO   IN-CNT
     END-READ.
*
 900-WK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
