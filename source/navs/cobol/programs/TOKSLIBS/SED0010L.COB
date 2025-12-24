****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩＣ　　　　　　　　　　　　　*
*    モジュール名　　　　：　受領書発行　　　　　　　　　　　　*
*    作成日／更新日　　　：　2015/08/20                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＥＤＩＣ受領ワークよりＥＤＩＣ    *
*                        ：　受領書を出力する　　　　　　　　  *
*    更新日／更新者　　　：　2020/06/19 NAV TAKAHASHI          *
*    更新概要　　　　　　：　ＪＡＮＣＤの出力桁数変更　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION            DIVISION.
****************************************************************
 PROGRAM-ID.               SED0010L.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             2015/08/21.
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
*----<< EDIC受領ワーク >>--*
     SELECT     EWJYURF    ASSIGN    TO        DA-01-VI-EWJYURL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       JWK-F013
                                               JWK-F04
                                               JWK-F07
                                               JWK-F03
                                               JWK-F12
                           FILE      STATUS    JWK-ST.
*----<< ﾌﾟﾘﾝﾀｰ >>--*
     SELECT     PRTF       ASSIGN    TO        LP-04-PRTF.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
*----<< EDIC受領ワーク >>--*
 FD  EWJYURF     LABEL       RECORD    IS        STANDARD.
     COPY        EWJYURF   OF        XFDLIB
     JOINING     JWK       AS        PREFIX.
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
     03  JWK-ST              PIC  X(02).
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
     03  JWK-ERR             PIC  N(10) VALUE
         NC"受領書ワークエラー".
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
     02  NEW-DENNO           PIC  9(07).
*
 01  OLD-KEY.
     02  OLD-TENCD           PIC  9(08).
     02  OLD-DENNO           PIC  9(07).
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
     02  FILLER              PIC  X(08)  VALUE  "SED0010L".
     02  FILLER              PIC  X(43)  VALUE  SPACE.
     02  FILLER              PIC  N(08)
         CHARACTER  TYPE  IS  YB-22 VALUE  NC"＜　受領書　＞".
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
         NC"発注数".
     02  FILLER              PIC  X(02)  VALUE
         "( ".
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"出荷数".
     02  FILLER              PIC  X(03)  VALUE
         ")( ".
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"受領数".
     02  FILLER              PIC  X(01)  VALUE
         ")".
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
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"税区".
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
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TENCD               PIC  ZZZZ9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TENNM               PIC  N(20)
         CHARACTER  TYPE  IS  YB.
*
****  明細行２
 01  MEISAI-2.
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  HACCHUKUBUN         PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  HACCHUKUBUNMEI      PIC  N(06)
         CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  KEIJOBI             PIC  9(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  DEMPYONO            PIC  9(09).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  GYONO               PIC  9(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHOHINCODE          PIC  X(13).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  SHOHINMEI           PIC  X(20).
     02  HACHUSU             PIC  -,---,--9.
     02  SHUKKASU            PIC  -,---,--9.
     02  JURYOSU             PIC  -,---,--9.
     02  GENTANKA            PIC  -,---,--9.
     02  GENKAKINGAKU        PIC  ---,---,--9.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZEIKUBUN            PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  ZEIKUBUNMEI         PIC  X(05).
*
****  伝票計
 01  GOKEI-1.
     02  FILLER              PIC  X(59)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
         NC"＜伝票合計＞"      CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(11)  VALUE  SPACE.
     02  HACHUSU1            PIC  -,---,--9.
     02  SHUKKASU1           PIC  -,---,--9.
     02  JURYOSU1            PIC  -,---,--9.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  GENKAKINGAKU1       PIC  ---,---,--9.
*
****  店舗計
 01  GOKEI-2.
     02  FILLER              PIC  X(59)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
         NC"＜店舗合計＞"      CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(11)  VALUE  SPACE.
     02  HACHUSU2            PIC  -,---,--9.
     02  SHUKKASU2           PIC  -,---,--9.
     02  JURYOSU2            PIC  -,---,--9.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  GENKAKINGAKU2       PIC  ---,---,--9.
*
****  総合計
 01  GOKEI-3.
     02  FILLER              PIC  X(59)  VALUE  SPACE.
     02  FILLER              PIC  N(06)  VALUE
         NC"　＜総合計＞"      CHARACTER  TYPE  IS  YB.
     02  FILLER              PIC  X(11)  VALUE  SPACE.
     02  HACHUSU3            PIC  -,---,--9.
     02  SHUKKASU3           PIC  -,---,--9.
     02  JURYOSU3            PIC  -,---,--9.
     02  FILLER              PIC  X(09)  VALUE  SPACE.
     02  GENKAKINGAKU3       PIC  ---,---,--9.
*
 01  DKEI-AREA.
     02  DHACSU              PIC  9(07).
     02  DSHUSU              PIC  9(07).
     02  DJUCSU              PIC  9(07).
     02  DKINGK              PIC  9(09).
*
 01  TKEI-AREA.
     02  THACSU              PIC  9(07).
     02  TSHUSU              PIC  9(07).
     02  TJUCSU              PIC  9(07).
     02  TKINGK              PIC  9(09).
*
 01  SKEI-AREA.
     02  SHACSU              PIC  9(07).
     02  SSHUSU              PIC  9(07).
     02  SJUCSU              PIC  9(07).
     02  SKINGK              PIC  9(09).
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
 JWK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE EWJYURF.
     DISPLAY     JWK-ERR   UPON      CONS.
     DISPLAY     JWK-ST    UPON      CONS.
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
     PERFORM     DEN-READ-SEC
                 UNTIL END-SW  = 1  OR
                       HIT-FLG = 1.
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
*                      Ｉ Ｎ Ｉ Ｔ
**********************************************************
 INIT-SEC                  SECTION.
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
     OPEN        INPUT     EWJYURF
                 OUTPUT    PRTF.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理
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
     CLOSE       EWJYURF.
     CLOSE       PRTF.
*
*    DISPLAY  "SED0010L READ =" READ-CNT UPON STA.
*    DISPLAY  "SED0010L IN   =" IN-CNT   UPON STA.
 END-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し
**********************************************************
 SYUKEI-SEC                   SECTION.
*
*----<< ﾃﾞﾝﾋﾟｮｳｹｲ >>--*
     ADD       JWK-F21            TO        DHACSU.
     ADD       JWK-F22            TO        DSHUSU.
     ADD       JWK-F23            TO        DJUCSU.
     ADD       JWK-F17            TO        DKINGK.
*
*----<< ﾃﾝﾎﾟｹｲ >>--*
     ADD       JWK-F21            TO        THACSU.
     ADD       JWK-F22            TO        TSHUSU.
     ADD       JWK-F23            TO        TJUCSU.
     ADD       JWK-F17            TO        TKINGK.
*
*----<< ｿｳｺﾞｳｹｲ >>--*
     ADD       JWK-F21            TO        SHACSU.
     ADD       JWK-F22            TO        SSHUSU.
     ADD       JWK-F23            TO        SJUCSU.
     ADD       JWK-F17            TO        SKINGK.
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
     MOVE      SYS-DATE(1:4)      TO        SYSYY.
     MOVE      WK-M               TO        SYSMM.
     MOVE      WK-D               TO        SYSDD.
*    MOVE      WK-TIME(1:2)       TO        TIMEHH.
*    MOVE      WK-TIME(3:2)       TO        TIMEMM.
*    MOVE      WK-TIME(5:2)       TO        TIMESS.
*
*----<< ｼﾞｭｼﾝﾋﾞ ﾄﾘﾋｷｻｷﾒｲ >>--*
     MOVE      JWK-F011           TO        JUSHINBI.
     MOVE      JWK-F013           TO        TORICD.
     MOVE      JWK-F02            TO        TORINAM.
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
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
*
     MOVE      SPACE              TO        MEISAI-1.
     MOVE      SPACE              TO        MEISAI-2.

*日付種類
     IF  DEN-FLG = 0
          MOVE  JWK-F04(9:5)      TO        TEMPOCD-MOJI
          MOVE  TEMPOCD-SUCHI     TO        TENCD
          MOVE  JWK-F05           TO        TENNM
          MOVE  JWK-F08           TO        HACCHUKUBUN
          MOVE  JWK-F09           TO        HACCHUKUBUNMEI
          MOVE  JWK-F07           TO        KEIJOBI
          MOVE  JWK-F03           TO        DEMPYONO-MOJI
          MOVE  DEMPYONO-SUCHI    TO        DEMPYONO
          MOVE  JWK-F10           TO        ZEIKUBUN
          MOVE  SPACE             TO        ZEIKUBUNMEI
     END-IF.
*
     MOVE      JWK-F12            TO        GYONO-MOJI.
     MOVE      GYONO-SUCHI        TO        GYONO.
*# 2020/06/19 NAV ST
*****MOVE      JWK-F14            TO        SHOHINCODE.
     MOVE      JWK-F14(2:13)      TO        SHOHINCODE.
*# 2020/06/19 NAV ED
     MOVE      JWK-F15            TO        SHOHINMEI.
     MOVE      JWK-F21            TO        HACHUSU.
     MOVE      JWK-F22            TO        SHUKKASU.
     MOVE      JWK-F23            TO        JURYOSU.
     MOVE      JWK-F18            TO        GENTANKA.
     MOVE      JWK-F17            TO        GENKAKINGAKU.
*
**************
*帳票書き出し
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
*                    伝票合計書き出し
**********************************************************
 DENWRT-SEC                   SECTION.
*
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
     MOVE      DHACSU             TO        HACHUSU1.
     MOVE      DSHUSU             TO        SHUKKASU1.
     MOVE      DJUCSU             TO        JURYOSU1.
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
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
     MOVE      THACSU             TO        HACHUSU2.
     MOVE      TSHUSU             TO        SHUKKASU2.
     MOVE      TJUCSU             TO        JURYOSU2.
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
     IF  LINE-CNT       >         57
         PERFORM    HEAD-WRT-SEC
     END-IF.
     MOVE      SHACSU             TO        HACHUSU3.
     MOVE      SSHUSU             TO        SHUKKASU3.
     MOVE      SJUCSU             TO        JURYOSU3.
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
*    全角変換
****************************************************************
*ZENKAKU-SEC          SECTION.
* 日本語変換する。
*    MOVE   SPACE  TO  HENKAN-FLG.
*    PERFORM  VARYING IX  FROM 1 BY 1 UNTIL   IX > 8
*    IF ( WK-KENSU-X(IX:1) NOT = 0 ) AND
*       ( WK-KENSU-X(IX:1) NOT = SPACE )
*        MOVE "SET"   TO  HENKAN-FLG
*    END-IF
*        EVALUATE    WK-KENSU-X(IX:1)
*            WHEN    0
*                IF HENKAN-FLG = "SET"
*                    MOVE NC"０"  TO   WK-HENKAN-NR(IX)
*                ELSE
*                    MOVE NC"  "  TO   WK-HENKAN-NR(IX)
*                END-IF
*            WHEN    1
*                MOVE NC"１"  TO   WK-HENKAN-NR(IX)
*            WHEN    2
*                MOVE NC"２"  TO   WK-HENKAN-NR(IX)
*            WHEN    3
*                MOVE NC"３"  TO   WK-HENKAN-NR(IX)
*            WHEN    4
*                MOVE NC"４"  TO   WK-HENKAN-NR(IX)
*            WHEN    5
*                MOVE NC"５"  TO   WK-HENKAN-NR(IX)
*            WHEN    6
*                MOVE NC"６"  TO   WK-HENKAN-NR(IX)
*            WHEN    7
*                MOVE NC"７"  TO   WK-HENKAN-NR(IX)
*            WHEN    8
*                MOVE NC"８"  TO   WK-HENKAN-NR(IX)
*            WHEN    9
*                MOVE NC"９"  TO   WK-HENKAN-NR(IX)
*        END-EVALUATE
*    END-PERFORM.
*ZENKAKU-EXIT.
*    EXIT.
****************************************************************
*             ワーク読込み
****************************************************************
 DEN-READ-SEC            SECTION.
*
     READ     EWJYURF  NEXT  AT  END
              MOVE     1         TO   END-SW
              MOVE     99999999  TO   NEW-TENCD
              MOVE     9999999   TO   NEW-DENNO
              GO                 TO   DEN-READ-EXIT
     END-READ.
*
     MOVE     JWK-F04(9:5)       TO   TEMPOCD-MOJI.
     MOVE     TEMPOCD-SUCHI      TO   NEW-TENCD.
     MOVE     JWK-F03            TO   DEMPYONO-MOJI.
     MOVE     DEMPYONO-SUCHI     TO   NEW-DENNO.
     ADD      1                  TO   IN-CNT.
     MOVE     1                  TO   HIT-FLG.
*
 DEN-READ-EXIT.
     EXIT.
