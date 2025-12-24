****************************************************************
*    顧客名　　　　　　　：　_サカタのタネ殿　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ新ＥＤＩシステム            *
*    モジュール名　　　　：　ナフコ受領アンマッチデータ作成    *
*    作成日／更新日　　　：　12/04/02                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　出荷実績と受領実績を突き合わせアン*
*                        ：　マッチデータを作成する。　　　　　*
*            更新日　　　：　12/05/30                          *
*                        ：  OUTPUTに「サカタ伝票区分」を追加
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3717B.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          12/04/02.
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
*----<< 出荷実績データ >>--*
     SELECT   NFJISKF  ASSIGN          DA-01-VI-NFJISKL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  JIS-F04  JIS-F05  JIS-F06
                        STATUS         JIS-ST.
*----<< 受領実績データ >>--*
     SELECT   NFJUJKF    ASSIGN        DA-01-VI-NFJUJKL2
                        ORGANIZATION   INDEXED
*I
*                       ACCESS    MODE SEQUENTIAL
                        ACCESS    MODE RANDOM
*I
                        RECORD    KEY  JUS-F04  JUS-F05  JUS-F06
                        STATUS         JUS-ST.
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
 FD  NFJISKF             LABEL RECORD   IS   STANDARD.
     COPY     NFJISKF   OF        XFDLIB
              JOINING   JIS       PREFIX.
*----<< 受領実績データ >>--*
 FD  NFJUJKF             LABEL RECORD   IS   STANDARD.
     COPY     NFJUJKF   OF        XFDLIB
              JOINING   JUS       PREFIX.
*----<< 受領アンマッチデータ >>--*
 FD  NFNUNMF            LABEL RECORD   IS   STANDARD.
     COPY     NFNUNMF   OF        XFDLIB
              JOINING   UNM       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  JIS-END        PIC  9(01)     VALUE  ZERO.
     03  JUS-END        PIC  9(01)     VALUE  ZERO.
     03  INV-FLG        PIC  9(01)     VALUE  ZERO.
     03  CHK-FLG        PIC  X(01)     VALUE  SPACE.
     03  OUT-FLG        PIC  9(01)     VALUE  ZERO.
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)     VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)     VALUE  ZERO.
     03  IN-CNT         PIC  9(09)     VALUE  ZERO.
*
     03  CNT-7          PIC  9(06)     VALUE  ZERO.
     03  CNT-8          PIC  9(06)     VALUE  ZERO.
     03  CNT-9          PIC  9(06)     VALUE  ZERO.
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  JIS-ST             PIC  X(02).
 01  JUS-ST             PIC  X(02).
 01  UNM-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  PG-ID              PIC  X(08)     VALUE   "SSY3717B".
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
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  JIS-KEY.
         05  JIS-TENPO  PIC  9(05)    VALUE  ZERO.
         05  JIS-DENNO  PIC  9(09)    VALUE  ZERO.
         05  JIS-GYO    PIC  9(02)    VALUE  ZERO.
     03  JUS-KEY.
         05  JUS-TENPO  PIC  9(05)    VALUE  ZERO.
         05  JUS-DENNO  PIC  9(09)    VALUE  ZERO.
         05  JUS-GYO    PIC  9(02)    VALUE  ZERO.
*
 01  WK-TORICD          PIC  9(08)    VALUE  ZERO.
*
*----<< ｶｳﾝﾄｴﾘｱ >>--*
 01  CNT-AREA.
     03  CNT-UNMATCH    PIC  9(07)    VALUE  ZERO.
     03  CNT-OK         PIC  9(07)    VALUE  ZERO.
     03  CNT-JIS        PIC  9(07)    VALUE  ZERO.
     03  CNT-JUS        PIC  9(07)    VALUE  ZERO.
*
 LINKAGE                SECTION.
 01  PARA-KBN           PIC  X(01).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-KBN.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 出荷実績データ >>--*
 JIS-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFJISKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " NFJISKL2 ERROR " JIS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*----<< 受領実績データ >>--*
 SETGKFS2-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFJUJKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " NFJUJKL2 ERROR " JUS-ST " "
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
*I
*    PERFORM  200-MAIN-RTN   UNTIL     JIS-END   =    1
*                                  AND JUS-END   =    1.
     PERFORM  200-MAIN-RTN   UNTIL     JIS-END   =    1.
*I
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
     OPEN     INPUT     NFJISKF.
     OPEN     INPUT     NFJUJKF.
     OPEN     OUTPUT    NFNUNMF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     ZERO           TO   CNT-AREA.
     MOVE     ZERO           TO   JIS-END JUS-END.
     INITIALIZE         BREAK-KEY.
*
     PERFORM  900-JIS-READ.
     PERFORM  900-JUS-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     EVALUATE  TRUE
*I
*出荷なし
*        WHEN      JIS-KEY   >    JUS-KEY
*              MOVE     "1"       TO      CHK-FLG
*              PERFORM  210-HENSYU-RTN
*              PERFORM  900-JUS-READ
*I
*正常データ（金額アンマッチチェック）
         WHEN      JIS-KEY   =    JUS-KEY
               MOVE     "2"       TO      CHK-FLG
               PERFORM  210-HENSYU-RTN
               PERFORM  900-JIS-READ
               PERFORM  900-JUS-READ
*受領なし
*I
*        WHEN      JIS-KEY   <    JUS-KEY
         WHEN      INV-FLG   =    1
               MOVE     "3"       TO      CHK-FLG
               PERFORM  210-HENSYU-RTN
               PERFORM  900-JIS-READ
*I
               PERFORM  900-JUS-READ
*I
     END-EVALUATE.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    NFJISKF.
     CLOSE    NFJUJKF.
     CLOSE    NFNUNMF.
*データ内容メッセージ
     DISPLAY "ｼﾞｭﾘｮｳﾃﾞｰﾀﾅｼ = "  CNT-JUS     UPON CONS.
     DISPLAY "ｼｭｯｶﾃﾞｰﾀ  ﾅｼ = "  CNT-JIS     UPON CONS.
     DISPLAY "ﾀﾝｶ･ｽｳﾘｮｳﾁｶﾞｲ= "  CNT-UNMATCH UPON CONS.
     DISPLAY "OKﾃﾞｰﾀ       = "  CNT-OK      UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      データ出力（レコード区分＝１，２）          *
*--------------------------------------------------------------*
 210-HENSYU-RTN        SECTION.
*アンマッチリスト出力
*I
*    IF  CHK-FLG  =  "1"
*********出荷なし
*        MOVE     SPACE          TO   UNM-REC
*        INITIALIZE                   UNM-REC
*        MOVE     JUS-F04        TO   UNM-F01
*        MOVE     JUS-F05        TO   UNM-F02
*        MOVE     JUS-F06        TO   UNM-F03
*        MOVE     JUS-F01        TO   UNM-F04
*        MOVE     JUS-F08        TO   UNM-F05
*        MOVE     JUS-F15        TO   UNM-F06
*        MOVE     JUS-F12        TO   UNM-F07
*        MOVE     JUS-F13        TO   UNM-F08
*        MOVE     JUS-F14        TO   UNM-F09
*        MOVE     JUS-F17        TO   UNM-F10
*        MOVE     JUS-F16        TO   UNM-F11
*        MOVE     JUS-F18        TO   UNM-F12
*        MOVE     JUS-F19        TO   UNM-F13
*        MOVE     JUS-F20        TO   UNM-F14
*        MOVE     JUS-F21        TO   UNM-F15
*        MOVE     JUS-F22        TO   UNM-F16
*        MOVE     "1"            TO   UNM-F26
*        MOVE     "1"            TO   UNM-F28
*        IF  JUS-F17 = 0 OR 2 OR 3
*            MOVE "1"            TO   UNM-F32
*        END-IF
*        WRITE  UNM-REC
*↓20120530
******** ADD       1             TO   CNT-UNMATCH  CNT-JIS
*        ADD       1             TO   CNT-JIS
*↑20120530
*    END-IF.
*
*I
     IF  CHK-FLG  =  "3"
*********受領なし
         MOVE     SPACE          TO   UNM-REC
         INITIALIZE                   UNM-REC
         MOVE     JIS-F04        TO   UNM-F01
         MOVE     JIS-F05        TO   UNM-F02
         MOVE     JIS-F06        TO   UNM-F03
         MOVE     JIS-F01        TO   UNM-F20
         MOVE     JIS-F08        TO   UNM-F21
         MOVE     JIS-F07        TO   UNM-F22
         MOVE     JIS-F12        TO   UNM-F23
         MOVE     JIS-F13        TO   UNM-F24
         MOVE     JIS-F14        TO   UNM-F25
         MOVE     "1"            TO   UNM-F27
         MOVE     "1"            TO   UNM-F29
         WRITE  UNM-REC
*↓20120530
******** ADD       1             TO   CNT-UNMATCH  CNT-JUS
         ADD       1             TO   CNT-JUS
*↑20120530
     END-IF.
*
     IF  CHK-FLG  =  "2"
*********マッチングＯＫ
*********　数量違い／原価／計上日チェック
         IF   JIS-F12  =  JUS-F12
         AND  JIS-F13  =  JUS-F13
         AND  JIS-F01  =  JUS-F01
              ADD      1              TO   CNT-OK
         ELSE
              MOVE     SPACE          TO   UNM-REC
              INITIALIZE                   UNM-REC
              MOVE     JUS-F04        TO   UNM-F01
              MOVE     JUS-F05        TO   UNM-F02
              MOVE     JUS-F06        TO   UNM-F03
              MOVE     JUS-F01        TO   UNM-F04
              MOVE     JUS-F08        TO   UNM-F05
              MOVE     JUS-F15        TO   UNM-F06
              MOVE     JUS-F12        TO   UNM-F07
              MOVE     JUS-F13        TO   UNM-F08
              MOVE     JUS-F14        TO   UNM-F09
              MOVE     JUS-F17        TO   UNM-F10
              MOVE     JUS-F16        TO   UNM-F11
              MOVE     JUS-F18        TO   UNM-F12
              MOVE     JUS-F19        TO   UNM-F13
              MOVE     JUS-F20        TO   UNM-F14
              MOVE     JUS-F21        TO   UNM-F15
*↓20120530
              MOVE     JUS-F22        TO   UNM-F16
*↑20120530
              MOVE     JIS-F01        TO   UNM-F20
              MOVE     JIS-F08        TO   UNM-F21
              MOVE     JIS-F07        TO   UNM-F22
              MOVE     JIS-F12        TO   UNM-F23
              MOVE     JIS-F13        TO   UNM-F24
              MOVE     JIS-F14        TO   UNM-F25
              MOVE     "1"            TO   UNM-F26
              MOVE     "1"            TO   UNM-F27
              IF  JIS-F12  NOT =  JUS-F12
                  MOVE "1"            TO   UNM-F30
              END-IF
              IF  JIS-F13  NOT =  JUS-F13
                  MOVE "1"            TO   UNM-F31
              END-IF
              IF  JUS-F17 = 0 OR 2 OR 3
                  MOVE "1"            TO   UNM-F32
              END-IF
              IF  JIS-F01  NOT =  JUS-F01
                  MOVE "1"            TO   UNM-F33
                  IF  PARA-KBN = "1"
                      GO              TO   210-HENSYU-RTN-EXIT
                  END-IF
              END-IF
              WRITE  UNM-REC
              ADD       1             TO   CNT-UNMATCH
         END-IF
     END-IF.
*
 210-HENSYU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    受領実績Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-JUS-READ           SECTION.
*I
*    IF       JUS-END   =   1
*             GO        TO        900-JUS-READ-EXIT
*    END-IF.
*    READ     NFJUJKF    AT  END
*             MOVE   HIGH-VALUE   TO   JUS-KEY
*             MOVE      1         TO   JUS-END
*             NOT AT END
*             MOVE      JUS-F04   TO   JUS-TENPO
*             MOVE      JUS-F05   TO   JUS-DENNO
*             MOVE      JUS-F06   TO   JUS-GYO
*    END-READ.
     MOVE     JIS-F04   TO        JUS-F04.
     MOVE     JIS-F05   TO        JUS-F05.
     MOVE     JIS-F06   TO        JUS-F06.
     READ     NFJUJKF
        INVALID
              MOVE      1         TO   INV-FLG
        NOT INVALID
              MOVE      0         TO   INV-FLG
              MOVE      JUS-F04   TO   JUS-TENPO
              MOVE      JUS-F05   TO   JUS-DENNO
              MOVE      JUS-F06   TO   JUS-GYO
     END-READ.
*
 900-JUS-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    出荷実績ファイル　 READ                      *
*--------------------------------------------------------------*
 900-JIS-READ           SECTION.
     IF       JIS-END   =   1
              GO        TO        900-JIS-READ-EXIT
     END-IF.
     READ     NFJISKF
         AT END
              MOVE      HIGH-VALUE     TO   JIS-KEY
              MOVE      1              TO   JIS-END
         NOT AT END
              MOVE      JIS-F04   TO   JIS-TENPO
              MOVE      JIS-F05   TO   JIS-DENNO
              MOVE      JIS-F06   TO   JIS-GYO
              ADD       1         TO   IN-CNT
     END-READ.
*
 900-JIS-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
