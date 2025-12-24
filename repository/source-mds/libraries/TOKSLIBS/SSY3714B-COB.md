# SSY3714B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3714B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　_サカタのタネ殿　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ新ＥＤＩシステム            *
*    モジュール名　　　　：　ナフコ受領売上状況確認　　　　    *
*    作成日／更新日　　　：　12/05/30                          *
*    作成者／更新者　　　：　T.MIURA                         *
*    処理概要　　　　　　：　受領累積と売上伝票から状況確認を　*
*                        ：　マッチデータへ作成する。　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3714B.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          12/05/30.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 受領累積データ >>--*
     SELECT   NFJYURF  ASSIGN          DA-01-VI-NFJYURL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  JYU-F01  JYU-F05
                                       JYU-F07  JYU-F08
                        STATUS         JYU-ST.
*----<< 売上伝票データ >>--*
     SELECT   SHTDENF    ASSIGN        DA-01-VI-SHTDENLM
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  DEN-F01  DEN-F02
                                       DEN-F04  DEN-F07
                                       DEN-F03
                        STATUS         DEN-ST.
*----<< 受領アンマッチデータ >>--*
     SELECT   NFNUNMF   ASSIGN         DA-01-VI-NFNUNML1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  UNM-F01  UNM-F02  UNM-F03
                        STATUS         UNM-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 出荷実績データ >>--*
 FD  NFJYURF             LABEL RECORD   IS   STANDARD.
     COPY     NFJYURF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*----<< 売上伝票データ >>--*
 FD  SHTDENF             LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 受領アンマッチデータ >>--*
 FD  NFNUNMF            LABEL RECORD   IS   STANDARD.
     COPY     NFNUNMF   OF        XFDLIB
              JOINING   UNM       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  JYU-END        PIC  9(01)     VALUE  ZERO.
     03  DEN-END        PIC  9(01)     VALUE  ZERO.
     03  INV-FLG        PIC  9(01)     VALUE  ZERO.
     03  CHK-FLG        PIC  X(01)     VALUE  SPACE.
     03  OUT-FLG        PIC  9(01)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  JYU-ST             PIC  X(02).
 01  DEN-ST             PIC  X(02).
 01  UNM-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  PG-ID              PIC  X(08)     VALUE   "SSY3714B".
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
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
 01  WK-DATE            PIC  9(06).
 01  FILLER             REDEFINES      WK-DATE.
     03  WK-YY          PIC  9(02).
     03  WK-MM          PIC  9(02).
     03  WK-DD          PIC  9(02).
*
*
 01  WK-TORICD          PIC  9(08)    VALUE  137607.
*
*----<< ｶｳﾝﾄｴﾘｱ >>--*
 01  CNT-AREA.
     03  CNT-UNMATCH    PIC  9(07)    VALUE  ZERO.
     03  CNT-OK         PIC  9(07)    VALUE  ZERO.
     03  CNT-JYU        PIC  9(07)    VALUE  ZERO.
     03  CNT-DEN        PIC  9(07)    VALUE  ZERO.
*
 LINKAGE                SECTION.
 01  PARA-DATE          PIC  9(08).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-DATE.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 受領累積データ >>--*
 JYU-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFJYURF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " NFJYURL3 ERROR " JYU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< 売上伝票データ >>--*
 DEN-ERR                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SHTDENLM ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< 受領アンマッチデータ >>--*
 UNM-ERR                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFNUNMF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " NFNUNML1 ERROR " UNM-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*プログラム開始メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*プログラムコントロール
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     JYU-END   =    1.
     PERFORM  300-END-RTN.
*プログラム終了メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     NFJYURF.
     OPEN     INPUT     SHTDENF.
     OPEN     OUTPUT    NFNUNMF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     MOVE     ZERO           TO   CNT-AREA.
     MOVE     ZERO           TO   JYU-END.
*
     MOVE     SPACE              TO        JYU-REC.
     INITIALIZE                            JYU-REC.
     MOVE     PARA-DATE          TO        JYU-F01.
     MOVE     ZERO               TO        JYU-F05.
     MOVE     ZERO               TO        JYU-F07.
     MOVE     ZERO               TO        JYU-F08.
     START  NFJYURF KEY IS >= JYU-F01  JYU-F05 JYU-F07 JYU-F08
            INVALID
            DISPLAY NC"＃対象データなし＃" UPON CONS
            MOVE   1             TO        JYU-END
            GO                   TO        100-INIT-RTN-EXIT
     END-START.
*
     PERFORM  NFJYURF-RD-SEC.
     IF       JYU-END   =   1
              DISPLAY NC"＃対象データ無し＃" UPON CONS
              GO   TO    100-INIT-RTN-EXIT.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
*初期化
     MOVE     SPACE               TO   DEN-REC.
     INITIALIZE                        DEN-REC.
     MOVE     WK-TORICD           TO   DEN-F01.
     MOVE     JYU-F07             TO   DEN-F02.
     MOVE     0                   TO   DEN-F04.
     MOVE     JYU-F05             TO   DEN-F07.
     MOVE     JYU-F08             TO   DEN-F03.
*
     PERFORM  SHTDENF-RD-SEC.
     PERFORM  210-HENSYU-RTN.
     PERFORM  NFJYURF-RD-SEC.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    NFJYURF.
     CLOSE    SHTDENF.
     CLOSE    NFNUNMF.
*データ内容メッセージ
     DISPLAY "ｼﾞｭﾘｮｳﾃﾞｰﾀ   = "  CNT-JYU     UPON CONS.
     DISPLAY "ｳﾘｱｹﾞﾃﾞｰﾀ ﾅｼ = "  CNT-DEN     UPON CONS.
     DISPLAY "ｷﾝｶﾞｸﾁｶﾞｲ    = "  CNT-UNMATCH UPON CONS.
     DISPLAY "OKﾃﾞｰﾀ       = "  CNT-OK      UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      明細行印字（レコード区分＝１，２）          *
*--------------------------------------------------------------*
 210-HENSYU-RTN        SECTION.
*アンマッチリスト出力
     IF  INV-FLG  =  0
*********売上なし
         MOVE     SPACE          TO   UNM-REC
         INITIALIZE                   UNM-REC
         MOVE     JYU-F05        TO   UNM-F01
         MOVE     JYU-F07        TO   UNM-F02
         MOVE     JYU-F08        TO   UNM-F03
         MOVE     JYU-F02        TO   UNM-F04
         MOVE     JYU-F10        TO   UNM-F05
         MOVE     JYU-F03        TO   UNM-F06
         MOVE     JYU-F12        TO   UNM-F07
         MOVE     JYU-F13        TO   UNM-F08
         MOVE     JYU-F14        TO   UNM-F09
         MOVE     JYU-F15        TO   UNM-F10
         MOVE     JYU-F04        TO   UNM-F11
         MOVE     JYU-F16        TO   UNM-F12
         MOVE     JYU-F17        TO   UNM-F13
         MOVE     JYU-F18        TO   UNM-F14
         MOVE     JYU-F19        TO   UNM-F15
         MOVE     JYU-F09        TO   UNM-F16
         MOVE     "1"            TO   UNM-F26
         MOVE     "1"            TO   UNM-F28
         IF  JYU-F15 = 0 OR 2 OR 3
             MOVE "1"            TO   UNM-F32
         END-IF
         WRITE  UNM-REC
         ADD       1             TO   CNT-DEN
     ELSE
*
*****マッチングＯＫ
****　数量違い／原価
         IF   JYU-F12  =  DEN-F15
         AND  JYU-F13  =  DEN-F172
              ADD      1              TO   CNT-OK
         ELSE
             MOVE     SPACE          TO   UNM-REC
             INITIALIZE                   UNM-REC
             MOVE     JYU-F05        TO   UNM-F01
             MOVE     JYU-F07        TO   UNM-F02
             MOVE     JYU-F08        TO   UNM-F03
             MOVE     JYU-F02        TO   UNM-F04
             MOVE     JYU-F10        TO   UNM-F05
             MOVE     JYU-F03        TO   UNM-F06
             MOVE     JYU-F12        TO   UNM-F07
             MOVE     JYU-F13        TO   UNM-F08
             MOVE     JYU-F14        TO   UNM-F09
             MOVE     JYU-F15        TO   UNM-F10
             MOVE     JYU-F04        TO   UNM-F11
             MOVE     JYU-F16        TO   UNM-F12
             MOVE     JYU-F17        TO   UNM-F13
             MOVE     JYU-F18        TO   UNM-F14
             MOVE     JYU-F19        TO   UNM-F15
             MOVE     JYU-F09        TO   UNM-F16
             MOVE     DEN-F112       TO   UNM-F20
             MOVE     DEN-F25(1:8)   TO   UNM-F21
             MOVE     DEN-F08        TO   UNM-F22
             MOVE     DEN-F15        TO   UNM-F23
             MOVE     DEN-F172       TO   UNM-F24
             MOVE     DEN-F173       TO   UNM-F25
             MOVE     "1"            TO   UNM-F26
             MOVE     "1"            TO   UNM-F27
             IF  JYU-F12  NOT =  DEN-F15
                 MOVE "1"            TO   UNM-F30
             END-IF
             IF  JYU-F13  NOT =  DEN-F172
                 MOVE "1"            TO   UNM-F31
             END-IF
             IF  JYU-F15 = 0 OR 2 OR 3
                 MOVE "1"            TO   UNM-F32
             END-IF
             WRITE  UNM-REC
             ADD       1             TO   CNT-UNMATCH
         END-IF
     END-IF.
*
 210-HENSYU-RTN-EXIT.
     EXIT.
****************************************************************
*    ナフコ受領累積ファイル読込　　　
****************************************************************
 NFJYURF-RD-SEC             SECTION.
*
     READ     NFJYURF
          NEXT  AT END
              MOVE     1          TO        JYU-END
              GO                  TO        NFJYURF-RD-EXIT
     END-READ.
*
     IF   JYU-F04  > 0
          GO    TO     NFJYURF-RD-SEC
     END-IF.
     ADD       1         TO        CNT-JYU.
*
     IF   CNT-JYU(5:3)  =  "000" OR "500"
          DISPLAY "CNT-JYU = " CNT-JYU UPON CONS
     END-IF.
*
     IF   JYU-F02  >  PARA-DATE
          MOVE     1          TO        JYU-END
     END-IF.
*
 NFJYURF-RD-EXIT.
     EXIT.
****************************************************************
*    売上伝票ファイル読込　　　
****************************************************************
 SHTDENF-RD-SEC             SECTION.
*
     READ     SHTDENF
          INVALID
              MOVE      0         TO        INV-FLG
          NOT  INVALID
              MOVE      1         TO        INV-FLG
     END-READ.
*
 SHTDENF-RD-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
