# SED0020L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SED0020L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩＣ　　　　　　　　　　　　　*
*    モジュール名　　　　：　返品明細書発行　　　　　　　　　　*
*    作成日／更新日　　　：　2015/08/25                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＥＤＩＣ返品明細書ワークよりＥＤ　*
*                        ：　ＩＣ返品明細書を出力する　　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION            DIVISION.
****************************************************************
 PROGRAM-ID.               SED0020L.
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
*----<< EDIC返品明細書ワーク >>--*
     SELECT     EWHENPF    ASSIGN    TO        DA-01-VI-EWHENPL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       HWK-F013
                                               HWK-F04
                                               HWK-F07
                                               HWK-F03
                                               HWK-F12
                           FILE      STATUS    HWK-ST.
*----<< ﾌﾟﾘﾝﾀｰ >>--*
     SELECT     PRTF       ASSIGN    TO        LP-04-PRTF.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
*----<< EDIC返品明細書ワーク >>--*
 FD  EWHENPF     LABEL     RECORD    IS        STANDARD.
     COPY        EWHENPF   OF        XFDLIB
     JOINING     HWK       AS        PREFIX.
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
     03  HWK-ST              PIC  X(02).
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
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
 01  TEMPOCD-MOJI           PIC   X(05).
 01  FILLER            REDEFINES      TEMPOCD-MOJI.
     03  TEMPOCD-SUCHI      PIC  9(05).
*
*----<< 伝票番号変換 >>--*
 01  DEMPYONO-MOJI          PIC   X(10).
 01  FILLER            REDEFINES      DEMPYONO-MOJI.
     03  DEMPYONO-SUCHI     PIC   9(10).
*
*----<< 行番号変換 >>--*
 01  GYONO-MOJI             PIC   X(04).
 01  FILLER            REDEFINES      GYONO-MOJI.
     03  GYONO-SUCHI        PIC   9(04).
*
 01  IN-DATA                 PIC  X(01).
 01  FILE-ERR.
     03  HWK-ERR             PIC  N(10) VALUE
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
 01  NEW-TENCD               PIC  X(05)  VALUE  SPACE.
 01  OLD-TENCD               PIC  X(05)  VALUE  SPACE.
 01  NEW-KEJOBI              PIC  X(08)  VALUE  SPACE.
 01  OLD-KEJOBI              PIC  X(08)  VALUE  SPACE.
 01  NEW-DENNO               PIC  X(10)  VALUE  SPACE.
 01  OLD-DENNO               PIC  X(10)  VALUE  SPACE.
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
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SED0020L".
     02  FILLER              PIC  X(43)  VALUE  SPACE.
     02  FILLER              PIC  N(08)
         CHARACTER  TYPE  IS  YB-22 VALUE  NC"＜返品明細書＞".
* 2023/11/19 ARK UPD START
*    02  FILLER              PIC  X(24)  VALUE  SPACE.
     02  FILLER              PIC  X(36)  VALUE  SPACE.
* 2023/11/19 ARK UPD END
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
****  見出し行２
 01  MIDASI-2.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  MID2-01             PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"受信日：".
     02  JUSHINBI            PIC  9(08).
*
****  見出し行３
 01  MIDASI-3.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"取引先：".
     02  TORICD              PIC  9(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TORINAM             PIC  N(16)
         CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(59)  VALUE  SPACE.
*
****  見出し行４
 01  MIDASI-4.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"店舗情報".
*
****  見出し行５
 01  MIDASI-5.
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(02)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"発区".
     02  FILLER              PIC  X(11)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"計上日".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
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
     02  FILLER              PIC  X(29)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"返品数".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"原単価".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"原価金額".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)
         CHARACTER  TYPE  IS  YB    VALUE
         NC"返区".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(02)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"税区".
* 2023/11/19 ARK ADD START
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  FILLER              PIC  N(06)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"譲渡年月１２".
* 2023/11/19 ARK ADD END
*
****  線１
 01  HASEN-1.
     02  FILLER              PIC  X(140) VALUE  ALL "=".
****  線２
 01  HASEN-2.
     02  FILLER              PIC  X(32) VALUE  SPACE.
     02  FILLER              PIC  X(108) VALUE  ALL "-".
****  線３
 01  HASEN-3.
     02  FILLER              PIC  X(140) VALUE  ALL "-".
*
****  明細行１
 01  MEISAI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TENCD               PIC  ZZZZ9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TENNM               PIC  N(20)
         CHARACTER  TYPE  IS  YB.
*
****  明細行２
 01  MEISAI-2.
* 2023/11/19 ARK UPD START
*    02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
* 2023/11/19 ARK UPD START
     02  HACCHUKUBUN         PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  HACCHUKUBUNMEI      PIC  N(06)
         CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  KEIJOBI             PIC  9(08).
* 2023/11/19 ARK UPD START
*    02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
* 2023/11/19 ARK UPD END
     02  DEMPYONO            PIC  9(09).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  GYONO               PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHOHINCODE          PIC  X(13).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHOHINMEI           PIC  X(20).
     02  HEMPINSU            PIC  -,---,--9.
     02  GENTANKA            PIC  -,---,--9.
     02  GENKAKINGAKU        PIC  ---,---,--9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  HEMPINKUBUN         PIC  X(03).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZEIKUBUN            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZEIKUBUNMEI         PIC  N(06)
         CHARACTER  TYPE  IS  YB.
* 2023/11/19 ARK ADD START
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  JYOTOYM1            PIC  X(06).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  JYOTOYM2            PIC  X(06).
* 2023/11/19 ARK ADD END
*
****  伝票計
 01  GOKEI-1.
     02  FILLER              PIC  X(54)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
         NC"＜伝票合計＞"      CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(16)  VALUE  SPACE.
     02  HEMPINSU1           PIC  -,---,--9.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  GENKAKINGAKU1       PIC  ---,---,--9.
*
****  店舗計
 01  GOKEI-2.
     02  FILLER              PIC  X(54)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
         NC"＜店舗合計＞"      CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(16)  VALUE  SPACE.
     02  HEMPINSU2           PIC  -,---,--9.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  GENKAKINGAKU2       PIC  ---,---,--9.
*
****  総合計
 01  GOKEI-3.
     02  FILLER              PIC  X(54)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
         NC"　＜総合計＞"      CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(16)  VALUE  SPACE.
     02  HEMPINSU3           PIC  -,---,--9.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  GENKAKINGAKU3       PIC  ---,---,--9.
*
 01  DKEI-AREA.
     02  DHENSU              PIC  9(07).
     02  DKINGK              PIC  9(09).
*
 01  TKEI-AREA.
     02  THENSU              PIC  9(07).
     02  TKINGK              PIC  9(09).
*
 01  SKEI-AREA.
     02  SHENSU              PIC  9(07).
     02  SKINGK              PIC  9(09).
*
****************************************************************
 PROCEDURE                 DIVISION.
****************************************************************
 DECLARATIVES.
 PRT-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     STOP        RUN.
 HWK-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE EWHENPF.
     DISPLAY     HWK-ERR   UPON      CONS.
     DISPLAY     HWK-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理
***********************************************************
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
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ
**********************************************************
 INIT-SEC                  SECTION.
*
*----<< 開始メッセージ出力 >>--*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SED0020L START *** "
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
*
*----<< ｼｽﾃﾑｼﾞｺｸ ｼｭﾄｸ >>--*
     ACCEPT    WK-TIME          FROM   TIME.
*
*----<<FILE OPEN >>--*
     OPEN        INPUT     EWHENPF
                 OUTPUT    PRTF.
*
*----<< ワーク読込 >>--*
     PERFORM  900-WK-READ.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理
***********************************************************
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
**********************************************************
*                       Ｅ Ｎ Ｄ
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
     END-IF.
*
*----<<FILE CLOSE >>--*
     CLOSE       PRTF.
     CLOSE       EWHENPF.
*
*----<< 件数出力 >>--*
     DISPLAY  "*** INPUT  = " IN-CNT " ***" UPON CONS.
*
*----<< 終了メッセージ出力 >>--*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SED0020L END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 END-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し
**********************************************************
 SYUKEI-SEC                   SECTION.
*
*----<< ﾃﾞﾝﾋﾟｮｳｹｲ >>--*
     ADD       HWK-F21            TO        DHENSU.
     ADD       HWK-F17            TO        DKINGK.
*
*----<< ﾃﾝﾎﾟｹｲ >>--*
     ADD       HWK-F21            TO        THENSU.
     ADD       HWK-F17            TO        TKINGK.
*
*----<< ｿｳｺﾞｳｹｲ >>--*
     ADD       HWK-F21            TO        SHENSU.
     ADD       HWK-F17            TO        SKINGK.
*
 SYUKEI-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し
**********************************************************
 HEAD-WRT-SEC                  SECTION.
*
*----<< ﾋﾂﾞｹ ｼﾞｶﾝ >>--*
*    MOVE      WK-YS              TO        YY1.
*    MOVE      WK-Y               TO        YY2.
     MOVE      LINK-OUT-YMD(1:4)  TO        SYSYY.
     MOVE      WK-M               TO        SYSMM.
     MOVE      WK-D               TO        SYSDD.
*    MOVE      WK-TIME(1:2)       TO        TIMEHH.
*    MOVE      WK-TIME(3:2)       TO        TIMEMM.
*    MOVE      WK-TIME(5:2)       TO        TIMESS.
*
*----<< ｼﾞｭｼﾝﾋﾞ ﾄﾘﾋｷｻｷﾒｲ >>--*
     MOVE      HWK-F011           TO        JUSHINBI.
     MOVE      HWK-F013           TO        TORICD.
     MOVE      HWK-F02            TO        TORINAM.
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
     WRITE     P-REC   FROM    MIDASI-5     AFTER  1.
     WRITE     P-REC   FROM    HASEN-1      AFTER  1.
*
     MOVE      10                 TO        LINE-CNT.
*
 HEAD-WRT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し
**********************************************************
 MEIWRT-SEC                  SECTION.
*
*----<< 変数クリア >>--*
     MOVE      SPACE              TO        MEISAI-1.
     MOVE      SPACE              TO        MEISAI-2.
*
*----<< 改ページ判定 >>--*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
*----<< 店舗コードのブレイク判定 >>--*
     IF    NEW-TENCD  NOT =  OLD-TENCD
           IF  IN-CNT  >  1
               PERFORM     DENWRT-SEC
               PERFORM     TENWRT-SEC
               MOVE  ZERO         TO        THENSU
                                            TKINGK
               MOVE  ZERO         TO        DHENSU
                                            DKINGK
           END-IF
           MOVE  HWK-F04(9:5)     TO        TEMPOCD-MOJI
                                            OLD-TENCD
           MOVE  TEMPOCD-SUCHI    TO        TENCD
           MOVE  HWK-F05          TO        TENNM
           MOVE  SPACE            TO        OLD-KEJOBI
           MOVE  HWK-F08          TO        HACCHUKUBUN
           MOVE  HWK-F09          TO        HACCHUKUBUNMEI
           MOVE  HWK-F03          TO        DEMPYONO-MOJI
                                            OLD-DENNO
           MOVE  DEMPYONO-SUCHI   TO        DEMPYONO
           MOVE  HWK-F10          TO        ZEIKUBUN
           MOVE  HWK-F11(1:6)     TO        ZEIKUBUNMEI
* 2023/11/19 ARK ADD START
           MOVE  HWK-F24          TO        JYOTOYM1
           MOVE  HWK-F25          TO        JYOTOYM2
* 2023/11/19 ARK ADD END
           WRITE     P-REC   FROM   MEISAI-1  AFTER  1
           ADD       1            TO        LINE-CNT
     END-IF.
*
*----<< 伝票番号のブレイク判定 >>--*
     IF    NEW-DENNO  NOT =  OLD-DENNO
           IF  IN-CNT  >  1
               PERFORM     DENWRT-SEC
               MOVE  ZERO         TO        DHENSU
                                            DKINGK
           END-IF
           MOVE  HWK-F08          TO        HACCHUKUBUN
           MOVE  HWK-F09          TO        HACCHUKUBUNMEI
           MOVE  HWK-F03          TO        DEMPYONO-MOJI
                                            OLD-DENNO
           MOVE  DEMPYONO-SUCHI   TO        DEMPYONO
           MOVE  HWK-F10          TO        ZEIKUBUN
           MOVE  HWK-F11(1:6)     TO        ZEIKUBUNMEI
     END-IF.
*
*----<< 計上日のブレイク判定 >>--*
     IF    NEW-KEJOBI  NOT =  OLD-KEJOBI
           MOVE  HWK-F07          TO        KEIJOBI
                                            OLD-KEJOBI
     END-IF.
*
     MOVE      HWK-F12            TO        GYONO-MOJI.
     MOVE      GYONO-SUCHI        TO        GYONO.
     MOVE      HWK-F14            TO        SHOHINCODE.
     MOVE      HWK-F15            TO        SHOHINMEI.
     MOVE      HWK-F21            TO        HEMPINSU.
     MOVE      HWK-F18            TO        GENTANKA.
     MOVE      HWK-F17            TO        GENKAKINGAKU.
     MOVE      HWK-F22            TO        HEMPINKUBUN.
* 2023/11/19 ARK ADD START
     MOVE      HWK-F24            TO        JYOTOYM1.
     MOVE      HWK-F25            TO        JYOTOYM2.
* 2023/11/19 ARK ADD END
*
**************
*帳票書き出し
**************
*
*----<< 改ページ判定 >>--*
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
*                    伝票合計書き出し
**********************************************************
 DENWRT-SEC                   SECTION.
*
*----<< 改ページ判定 >>--*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
     MOVE      DHENSU             TO        HEMPINSU1.
     MOVE      DKINGK             TO        GENKAKINGAKU1.
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
     WRITE      P-REC   FROM    HASEN-2     AFTER  1.
     ADD        1                 TO        LINE-CNT.
     MOVE       0                 TO        DEN-FLG.
     INITIALIZE                             DKEI-AREA.
     MOVE     NEW-DENNO           TO        OLD-DENNO.
*
 DENWRT-EXIT.
     EXIT.
**********************************************************
*                    店舗合計書き出し
**********************************************************
 TENWRT-SEC                   SECTION.
*
*----<< 改ページ判定 >>--*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
     MOVE      THENSU             TO        HEMPINSU2.
     MOVE      TKINGK             TO        GENKAKINGAKU2.
*
**************
*帳票書き出し
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
*                    総合計書き出し
**********************************************************
 SOUWRT-SEC                   SECTION.
*
*----<< 改ページ判定 >>--*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
     MOVE      SHENSU             TO        HEMPINSU3.
     MOVE      SKINGK             TO        GENKAKINGAKU3.
*
**************
*帳票書き出し
**************
     WRITE      P-REC   FROM    GOKEI-3    AFTER  1.
*
 SOUWRT-EXIT.
     EXIT.
****************************************************************
*             ワーク読込み
****************************************************************
 DEN-READ-SEC            SECTION.
*
     READ     EWHENPF  NEXT  AT  END
              MOVE     1         TO   END-SW
              MOVE     99999999  TO   NEW-TENCD
              MOVE     9999999   TO   NEW-DENNO
              GO                 TO   DEN-READ-EXIT
     END-READ.
*
     MOVE     HWK-F04(9:5)       TO   TEMPOCD-MOJI.
     MOVE     TEMPOCD-SUCHI      TO   NEW-TENCD.
     MOVE     HWK-F03            TO   DEMPYONO-MOJI.
     MOVE     DEMPYONO-SUCHI     TO   NEW-DENNO.
     ADD      1                  TO   IN-CNT.
     MOVE     1                  TO   HIT-FLG.
*
 DEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ワーク読込み
*--------------------------------------------------------------*
 900-WK-READ            SECTION.
     READ     EWHENPF
       AT END
              MOVE     "END"           TO   END-FLG
       NOT AT END
              MOVE     HWK-F04(9:5)    TO   NEW-TENCD
              MOVE     HWK-F07         TO   NEW-KEJOBI
              MOVE     HWK-F03         TO   NEW-DENNO
              ADD       1              TO   IN-CNT
     END-READ.
*
 900-WK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
