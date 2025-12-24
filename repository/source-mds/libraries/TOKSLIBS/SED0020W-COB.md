# SED0020W

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SED0020W.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩＣ　　　　　　　　　　　　　*
*    モジュール名　　　　：　返品ＭＳＧ抽出（共通）　　　　　　*
*    作成日／更新日　　　：　2015/08/25                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    ＩＮＰＵＴ　　　　　：　取引先コード PARA-TORICD          *
*                        ：　受信日       PARA-DATE            *
*                        ：　受信時刻     PARA-TIME            *
*    処理概要　　　　　　：　ＩＮＰＵＴパラメタより、ＥＤＩＣ  *
*                        ：　返品ＭＳＧを抽出する　　　　　　  *
*    更新日／更新者　　　：　2023/11/14 NAV TAKAHASHI          *
*    更新概要　　　　　　：　インボイス対応　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SED0020W.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/08/25.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< EDIC返品MSG >>--*
     SELECT   EDHENPF   ASSIGN         DA-01-VI-EDHENPL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  HEN-F011
                                       HEN-F012
                                       HEN-F013
                                       HEN-F02
                                       HEN-F03
                                       HEN-F04
                                       HEN-F05
                        STATUS         EDHENPF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52
                                       TEN-F011
                        STATUS         HTENMS-ST.
*----<< EDIC返品明細書ワーク >>--*
     SELECT   EWHENPF   ASSIGN    TO        DA-01-VS-EWHENPF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HWK-STATUS.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< EDIC返品MSG >>--*
 FD  EDHENPF            LABEL     RECORD   IS   STANDARD.
     COPY     EDHENPF   OF        XFDLIB
              JOINING   HEN       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< EDIC返品明細書ワーク >>--*
 FD  EWHENPF            LABEL     RECORD   IS   STANDARD.
     COPY     EWHENPF   OF        XFDLIB
              JOINING   HWK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  EDHENPF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HTENMS-ST         PIC  X(02).
 01  HWK-STATUS        PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  TOK-INVALID-FLG   PIC  X(01).
 01  TEN-INVALID-FLG   PIC  X(01).
*
*----<< ｶｳﾝﾄ >>--*
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
*----<< 行番号変換 >>--*
 01  GYONO-SUCHI            PIC   9(04).
 01  FILLER            REDEFINES      GYONO-SUCHI.
     03  GYONO-MOJI         PIC   X(04).
*
*----<< 店舗コード変換 >>--*
 01  TEMPOCD-MOJI           PIC   X(05).
 01  FILLER            REDEFINES      TEMPOCD-MOJI.
     03  TEMPOCD-SUCHI      PIC  9(05).
*
*----<< 数量変換 >>--*
 01  SURYO-MOJI             PIC   X(07).
 01  FILLER            REDEFINES      SURYO-MOJI.
     03  SURYO-SUCHI        PIC   9(06)V9(01).
*
*----<< 単価変換 >>--*
 01  TANKA-MOJI             PIC   X(10).
 01  FILLER            REDEFINES      TANKA-MOJI.
     03  TANKA-SUCHI        PIC   9(08)V9(02).
*
*----<< 金額変換 >>--*
 01  KINGAKU-MOJI           PIC   X(10).
 01  FILLER            REDEFINES      KINGAKU-MOJI.
     03  KINGAKU-SUCHI      PIC   9(10).
*
*----<< 名称マスタ取得用 >>--*
 01  MEISHO-AREA.
     03  MEISHO-TORICD      PIC   9(08).
     03  MEISHO-KUBUN       PIC   X(06).
     03  MEISHO-CODE        PIC   X(10).
     03  MEISHO-MEI         PIC   N(20).
     03  MEISHO-KANA        PIC   X(20).
     03  MEISHO-KEKKA       PIC   X(01).
*
*----<< ｺﾃｲｺｳﾓｸ >>--*
 01  SHORISHUBETSU-KUBUN    PIC   X(06)  VALUE "ED184".
 01  ZEI-KUBUN              PIC   X(06)  VALUE "ED201".
 01  HEMPIN-NEBIKI-RIYU     PIC   X(06)  VALUE "ED187".
*
*----<< ﾍﾝｽｳ >>--*
 01  WK-AREA.
     03  WK-SHORISHUBETSU   PIC   N(20)  VALUE SPACE.
     03  WK-SHORISHUBETSU-K PIC   X(20)  VALUE SPACE.
     03  WK-ZEIKUBUN        PIC   N(20)  VALUE SPACE.
     03  WK-ZEIKUBUN-K      PIC   X(20)  VALUE SPACE.
     03  WK-HEMPIN-NEBIKI   PIC   N(20)  VALUE SPACE.
     03  WK-HEMPIN-NEBIKI-K PIC   X(20)  VALUE SPACE.
     03  WK-MEISHO-KEKKA1   PIC   X(01)  VALUE SPACE.
     03  WK-MEISHO-KEKKA2   PIC   X(01)  VALUE SPACE.
     03  WK-MEISHO-KEKKA3   PIC   X(01)  VALUE SPACE.
     03  WK-TENPO-CD        PIC   9(13)  VALUE ZERO.

*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>--*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SED0020W".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0020W".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0020W".
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
*
 COPY     KEIJYRF   OF        XFDLIB.
*
 LINKAGE                SECTION.
 01  PARA-TORICD            PIC   9(08).
 01  PARA-DATE              PIC   9(08).
 01  PARA-TIME              PIC   9(04).
****************************************************************
 PROCEDURE              DIVISION USING  PARA-TORICD
                                        PARA-DATE
                                        PARA-TIME.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ＥＤＩＣ返品ＭＳＧ >>--*
 EDHENPF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      EDHENPF.
     MOVE      "EDHENPF"    TO   AB-FILE.
     MOVE      EDHENPF-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     MOVE      "HTOKMS"     TO   AB-FILE.
     MOVE      HTOKMS-ST    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     MOVE      "HTENMS"     TO   AB-FILE.
     MOVE      HTENMS-ST    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< EDIC返品明細書ワーク >>--*
 EWHENPF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      EWHENPF.
     MOVE      "EWHENPF"    TO   AB-FILE.
     MOVE      HWK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.

 END     DECLARATIVES.
****************************************************************
*                                                              *
****************************************************************
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
*
*----<< ｼｮｷﾁ ｾｯﾄ >--*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
*----<< ｼｮｷﾒｯｾｰｼﾞ ｼｭﾂﾘｮｸ >>--*
     DISPLAY  MSG-START UPON CONS.
*
*----<<FILE OPEN >>--*
     OPEN     INPUT     EDHENPF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    EWHENPF.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
*----<< ﾄﾘﾋｷｻｷMﾖﾐｺﾐ >>--*
     MOVE     PARA-TORICD     TO   TOK-F01.
     PERFORM  900-TOK-READ.
*
*----<< ｷｰｼﾞｮｳﾎｳ ｾｯﾄ >>--*
     MOVE     PARA-DATE       TO   HEN-F011.
     MOVE     PARA-TIME       TO   HEN-F012.
     MOVE     PARA-TORICD     TO   HEN-F013.
     MOVE     ZERO            TO   HEN-F02.
     MOVE     ZERO            TO   HEN-F03.
     MOVE     ZERO            TO   HEN-F04.
     MOVE     ZERO            TO   HEN-F05.
*
*----<< FILE START >>--*
     START    EDHENPF  KEY  >=   HEN-F011
                                 HEN-F012
                                 HEN-F013
                                 HEN-F02
                                 HEN-F03
                                 HEN-F04
                                 HEN-F05
        INVALID   KEY
           MOVE      9    TO   END-FG
             GO   TO   INIT-EXIT
     END-START.
*
*----<< 1ｹﾝﾒ ﾖﾐｺﾐ >>--*
     PERFORM  900-HEN-READ.

 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >>--*
     INITIALIZE                   WK-AREA.
*
*----<< ﾃﾝﾎﾟM ﾖﾐｺﾐ >>--*
     MOVE     PARA-TORICD     TO   TEN-F52.
     MOVE     HEN-F212(9:5)   TO   TEMPOCD-MOJI.
     MOVE     TEMPOCD-SUCHI   TO   TEN-F011.
     PERFORM  900-TEN-READ.

*----<< EDICﾒｲｼｮｳ ｼｭﾄｸ >>--*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     SHORISHUBETSU-KUBUN
                              TO   MEISHO-KUBUN.
     MOVE     HEN-F230        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-SHORISHUBETSU.
     MOVE     MEISHO-KANA     TO   WK-SHORISHUBETSU-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA1.
*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     ZEI-KUBUN       TO   MEISHO-KUBUN.
     MOVE     HEN-F231        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-ZEIKUBUN.
     MOVE     MEISHO-KANA     TO   WK-ZEIKUBUN-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA2.
*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     HEMPIN-NEBIKI-RIYU
                              TO   MEISHO-KUBUN.
     MOVE     HEN-F305        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-HEMPIN-NEBIKI.
     MOVE     MEISHO-KANA     TO   WK-HEMPIN-NEBIKI-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA3.
*
*----<< WK ｻｸｾｲ >>--*
     MOVE     SPACE          TO   HWK-REC.
     INITIALIZE                   HWK-REC.
*
     MOVE     PARA-DATE      TO   HWK-F011.
     MOVE     PARA-TIME      TO   HWK-F012.
     MOVE     PARA-TORICD    TO   HWK-F013.
     MOVE     TOK-F03        TO   HWK-F02.
     MOVE     HEN-F208       TO   HWK-F03.
     MOVE     HEN-F212       TO   HWK-F04.
     MOVE     TEN-F03        TO   HWK-F05.
     MOVE     HEN-F214       TO   HWK-F06.
*****MOVE     HEN-F228       TO   HWK-F07.
     MOVE     HEN-F03        TO   HWK-F07.
     MOVE     HEN-F230       TO   HWK-F08.
     MOVE     WK-SHORISHUBETSU
                             TO   HWK-F09.
     MOVE     HEN-F231       TO   HWK-F10.
     MOVE     WK-ZEIKUBUN    TO   HWK-F11.
     MOVE     HEN-F05        TO   GYONO-SUCHI.
     MOVE     GYONO-MOJI     TO   HWK-F12.
     MOVE     SPACE          TO   HWK-F13.
     MOVE     HEN-F306       TO   HWK-F14.
     MOVE     HEN-F310       TO   HWK-F15.
     MOVE     HEN-F311       TO   HWK-F16.
*原価金額
     MOVE     HEN-F315       TO   KINGAKU-MOJI.
     COMPUTE  HWK-F17        =    KINGAKU-SUCHI / 100.
*原単価
     MOVE     HEN-F314       TO   TANKA-MOJI.
     MOVE     TANKA-SUCHI    TO   HWK-F18.
*売価金額
     MOVE     HEN-F317       TO   KINGAKU-MOJI.
     COMPUTE  HWK-F19        =    KINGAKU-SUCHI / 100.
*売単価
     MOVE     HEN-F316       TO   KINGAKU-MOJI.
     COMPUTE  HWK-F20        =    KINGAKU-SUCHI / 100.
*返品数量（バラ）
     MOVE     HEN-F318       TO   SURYO-MOJI.
     MOVE     SURYO-SUCHI    TO   HWK-F21.
     MOVE     HEN-F305       TO   HWK-F22.
     MOVE     WK-HEMPIN-NEBIKI
                             TO   HWK-F23.
*##2023/11/14 NAV ST
*譲渡年月１，２
     MOVE     HEN-F319       TO   HWK-F24.
     MOVE     HEN-F320       TO   HWK-F25.
*
*----<< WK ｶｷｺﾐ >>--*
     WRITE    HWK-REC.
     ADD      1              TO   WRT-CNT.
*
*----<< ﾂｷﾞﾚｺｰﾄﾞ ﾖﾐｺﾐ >>--*
     PERFORM  900-HEN-READ.

 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
*----<<FILE CLOSE >>--*
     CLOSE     EDHENPF.
     CLOSE     HTOKMS.
     CLOSE     HTENMS.
     CLOSE     EWHENPF.
*
     IF  OUT-CNT = ZERO
         MOVE      4001         TO   PROGRAM-STATUS
     END-IF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    ＥＤＩＣ返品ＭＳＧ　 READ                    *
*--------------------------------------------------------------*
 900-HEN-READ           SECTION.
     MOVE     "900-HEN-READ"      TO   S-NAME.
     READ     EDHENPF
       AT END
           MOVE      9    TO   END-FG
       NOT AT END
           ADD       1    TO   RD-CNT
     END-READ.
*
     IF (HEN-F011 = PARA-DATE AND
         HEN-F012 = PARA-TIME AND
         HEN-F013 = PARA-TORICD )
     THEN
         CONTINUE
     ELSE
           MOVE      9    TO   END-FG
     END-IF.
 900-HEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     "900-TOK-READ"      TO   S-NAME.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　 READ                            *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     MOVE     "900-TEN-READ"      TO   S-NAME.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    名称マスタサブルーチン                       *
*--------------------------------------------------------------*
 900-MEISHO-SUB         SECTION.
     CALL    "SED9001B" USING     MEISHO-TORICD
                                  MEISHO-KUBUN
                                  MEISHO-CODE
                                  MEISHO-MEI
                                  MEISHO-KANA
                                  MEISHO-KEKKA.
 900-MEISHO-SUB-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
